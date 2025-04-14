#include <A-public>
#undef link
#include <tapestry.h>
#include <A-init>
#include <sys/stat.h>
#include <utime.h>

#define get_type(T0) T0
#define tapestry(...)   new(tapestry,   __VA_ARGS__)
#define import(...)     new(import,     __VA_ARGS__)
#define flag(...)       new(flag,       __VA_ARGS__)
static path   src;
static path   install;
static path   include;
static string dbg;
static string directive;
#if defined(__linux__)
static symbol platform = "linux";
#elif defined(_WIN32)
static symbol platform = "windows";
#elif defined(__APPLE__)
static symbol platform = "darwin";
#endif

string import_cast_string(import a) {
    string config = string(alloc, 128);
    each (a->config, string, conf) {
        if (len(config))
            append(config, " ");
        concat(config, conf);
    }
    return config;
}

string flag_cast_string(flag a) {
    string res = string(alloc, 64);

    if (a->is_cflag)
        concat(res, a->name);
    else {
        if (a->is_static)
            append(res, "-Wl,-Bstatic ");
        concat(res, form(string, "-l%o", a->name));
        if (a->is_static)
            append(res, " -Wl,-Bdynamic");
    }
    return res;
}

bool is_debug(string name) {
    string f = form(string, ",%o,", dbg);
    string s = form(string, ",%o,", name);
    return strstr(f->chars, s->chars) != null;
}

none tapestry_with_path(tapestry a, path f) {
    a->imports      = array(64);
    a->project_path = directory(f);
    a->name         = filename(a->project_path);
    cstr build_dir  = is_debug(a->name) ? "debug" : "release";
    a->build_path   = form(path, "%o/%s", a->project_path, build_dir);
    a->app          = array(64);
    a->test               = array(64);
    a->lib                = array(64);
    map environment       = map();
    array  lines          = read(f, typeid(array));
    import im             = null;
    string last_platform  = null;
    string last_directive = null;
    string top_directive  = null;
    bool   is_import      = true;

    /// for each line, process by indentation level, and evaluate tokens
    set(environment, string("PROJECT"),      a->name);
    set(environment, string("PROJECT_PATH"), a->project_path);
    set(environment, string("BUILD_PATH"),   a->build_path);
    each(lines, line, l) {
        string first = get(l->text, 0);
        i32    flen  = len(first);
        if (l->indent == 0) {
            verify(first->chars[flen - 1] == ':', "expected base-directive and ':'");
            last_directive = mid(first, 0, flen - 1);
            verify(cmp(last_directive, "import") == 0 ||
                   cmp(last_directive, "lib")    == 0 ||
                   cmp(last_directive, "app")    == 0 ||
                   cmp(last_directive, "test")   == 0, "expected import or target directive");
            is_import = cmp(last_directive, "import") == 0;
            top_directive = copy(last_directive);
            set(environment, string("DIRECTIVE"), top_directive);
        } else if (l->indent == 1) {
            if (is_import) {
                string name   = evaluate(get(l->text, 0), environment);
                string uri    = evaluate(get(l->text, 1), environment);
                string commit = evaluate(get(l->text, 2), environment);
                im = import(
                    tapestry, a,
                    name,   name,
                    uri,    uri,
                    commit, commit,
                    environment, map(),
                    config, array(64), commands, array(16));
                push(a->imports, im);
                last_platform = null;
            } else {
                /// we need pre-build scripts too, to generate headers and such
                array flags =
                    cmp(top_directive, "lib")  == 0 ? a->lib  :
                    cmp(top_directive, "app")  == 0 ? a->app  : 
                    cmp(top_directive, "test") == 0 ? a->test : null;
                verify(flags, "invalid directive: %o", top_directive);
                each (l->text, string, w) {
                    pairs(environment, i) {
                        string name = i->key;
                        string value = i->value;
                    }
                    string t = evaluate(w, environment); // PROJECT is missing null char
                    if (len(t)) {
                        bool is_cflag  = t->chars[0] == '-';
                        bool is_static = t->chars[0] == '@';
                        push(flags, flag(name, t,
                            is_static, is_static, is_cflag, is_cflag));
                    }
                }
            }
        } else {
            if (first->chars[flen - 1] == ':') {
                last_platform = mid(first, 0, flen - 1);
            } else if (!last_platform || cmp(last_platform, platform) == 0) {
                if (cmp(first, ">") == 0) {
                    // combine tokens into singular string
                    string cmd = string(alloc, 64);
                    each(l->text, string, w) {
                        if (len(cmd))
                            append(cmd, " ");
                        concat(cmd, evaluate(w, environment));
                    }
                    push(im->commands, cmd);
                } else {
                    each(l->text, string, w) {
                        /// if its VAR=VAL then this is not 'config' but rather environment
                        char f = w->chars[0];
                        if (isalpha(f)) {
                            cstr e = strstr(w->chars, "=");
                            char name[128];
                            sz   ln = (sz)(e - w->chars);
                            verify(ln < 128, "invalid data");
                            memcpy(name, w->chars, ln);
                            name[ln] = 0;
                            char value[128];
                            sz value_ln = len(w) - ln - 1;
                            memcpy(value, &w->chars[ln + 1], value_ln);
                            string n   = trim(string(name));
                            string val = trim(string(value));
                            set(im->environment, n, val);
                        } else
                            push(im->config, evaluate(w, environment));
                    }
                }
            }
        }
    }
}

/// handle autoconfig (if present) and resultant / static Makefile
bool import_make(import im) {
    i64 conf_status = INT64_MIN;
    struct stat build_token, installed_token;
    bool debug      = is_debug(im->name);
    path t0 = form(path, "tapestry-token");
    path t1 = form(path, "%o/tokens/%o", install, im->name);

    make_dir(im->build_path);
    cd(im->build_path);
    string env = serialize_environment(im->environment, false);
    
    if (file_exists("%o", t0) && file_exists("%o", t1)) {
        /// get modified date on token, compare it to one in install dir
        int istat_build   = stat(cstring(t0), &build_token);
        int istat_install = stat(cstring(t1), &installed_token);
        if (istat_build == 0 && istat_install)
            conf_status = (i64)(build_token.st_mtime - installed_token.st_mtime);
    }

    if (conf_status == 0)
        return true;

    /// GN support
    if (file_exists("../DEPS")) {
        cstr   prev = getenv("PATH");

        /// clone depot tools if we need it
        if (!strstr(prev, "depot_tools")) {
            if (!dir_exists("%o/depot_tools", src)) {
                string cmd = form(string, "cd %o && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git", src);
                verify(system(cstring(cmd)) == 0, "depot_tools");
            }
            /// it may just need to be set in the PATH again, because this wont persist if they user does not have it
            string new_path = form(string, "%o/depot_tools:%o", src, prev);
            setenv("PATH", cstring(new_path), 1);
        }
        cd(im->import_path);
    
        /// now call gclient sync in the project folder
        verify(system("gclient sync -D") == 0, "gclient");
        verify(system("python3 tools/git-sync-deps") == 0, "git-sync-deps");
        string cmd = form(string, "bin/gn gen %s --args='%o'",
            debug ? "debug" : "release", im, im->config);
        verify(system(cstring(cmd)) == 0, "gn config");

    } else if (file_exists("../Cargo.toml")) {
        string cmd = form(string, "cargo build --%s --manifest-path ../Cargo.toml --target-dir .",
            debug ? "debug" : "release");
        verify(system(cstring(cmd)) == 0, "rust compilation");

    } else if (file_exists("../CMakeLists.txt")) {
        /// configure
        if (!file_exists("CMakeCache.txt")) {
            string cmd = form(string, "cmake -B . -S .. %o", im);
            int  iconf = system(cstring(cmd));
            verify(iconf == 0, "%o: configure failed", im->name);
        }
    
        /// build & install
        string  cmd = form(string, "%o cmake --build .", env);
        int    icmd = system(cstring(cmd));
        string inst = form(string, "%o cmake --install .", env);
        int   iinst = system(cstring(inst));

    } else {
        cstr Makefile = "Makefile";
        if (!file_exists("Makefile")) {
            if (file_exists("../configure.ac") || file_exists("../configure") || file_exists("../config")) {
                /// generate configuration scripts if available
                

                pairs(im->environment, i) {
                    string name = i->key;
                    string value = i->value;
                    print("env[%o] = %o", name, value);
                }

                if (!file_exists("../configure") && file_exists("../configure.ac")) {
                    verify(system("autoupdate ..")    == 0, "autoupdate");
                    verify(system("autoreconf -i ..") == 0, "autoreconf");
                }
                /// prefer our pre/generated script configure, fallback to config
                cstr configure = file_exists("../configure") ? "../configure" : "../config";
                if (file_exists("%s", configure)) {
                    string cmd_conf = form(string, "%o %s%s --prefix=%o %o",
                        env,
                        configure,
                        debug ? " --enable-debug" : "",
                        install,
                        im);
                    printf("---------------------------\n");
                    print("configure: %o", cmd_conf);
                    printf("---------------------------\n");
                    verify(system(cstring(cmd_conf)) == 0, configure);
                }
            }
        }
        if (file_exists("%s", Makefile)) {
            string make = form(string, "%o make -f %s install", env, Makefile);
            verify(system(cstring(make)) == 0, "%o", make);
        }
    }

    cd(im->build_path);
    if (dir_exists("share")) {
        /// copy this to our build folder, we must accumulate 'share' resources into our own
        /// when we package apps, this accumulation of share is desired
        /// its not a practical thing to crawl-through multiple overlapping projects that exist in different folders
        path from = form(path, "share");
        path to   = form(path, "%o/share", im->tapestry->build_path);
        cp(from, to, true, true);
    }

    /// create tokens to indicate no errors during config/build/install
    /// we place two: one in the build folder and one in the tokens folder
    /// their modified timestamps must match, and their files must be newer
    /// than anything in the build folder
    cstr both[2] = { cstring(t0), cstring(t1) };
    for (int i = 0; i < 2; i++) {
        FILE* ftoken = fopen(both[i], "wb");
        fwrite("im-a-token", 10, 1, ftoken);
        fclose(ftoken);
    }
    int istat_build   = stat(cstring(t0), &build_token);
    int istat_install = stat(cstring(t1), &installed_token);
    struct utimbuf times;
    times.actime  = build_token.st_atime;  // access time
    times.modtime = build_token.st_mtime;  // modification time
    utime(cstring(t1), &times);
    return true;
}

static bool is_dbg(cstr name) {
    cstr dbg = getenv("DBG");
    return dbg ? strstr(dbg, name) != 0 : false;
}

i32 tapestry_import(tapestry a) {
    /// make checkouts or symlink, then build & install
    each (a->imports, import, im) {
        bool   debug      = is_debug(im->name);
        symbol build_type = debug ? "debug" : "release";
        path checkout     = form(path, "%o/checkout", install);
        im->import_path   = form(path, "%o/%o", checkout, im->name);
        im->build_path    = form(path, "%o/%s", im->import_path, build_type);

        if (!dir_exists("%o/%o", checkout, im->name)) {
            print("checkout = %o", checkout);
            cd(checkout);
            if (A_len(src) && dir_exists("%o/%o", src, im->name)) {
                string cmd = form(string, "ln -s %o/%o %o/%o",
                    src, im->name, checkout, im->name);
                verify (system(cstring(cmd)) == 0, "symlink");
            } else {
                print("cwd = %o", path_cwd(2048));
                string cmd = form(string, "git clone %o %o --no-checkout && cd %o && git checkout %o && cd ..",
                    im->uri, im->name, im->name, im->commit);
                verify (system(cstring(cmd)) == 0, "git clone");
            }
        }
        string n = im->name;
        i32* c = null, *b = null, *i = null;
        make_dir(im->build_path);
        cd(im->build_path);
        make(im);
    }
    return 0;
}

i32 tapestry_install(tapestry a) {
    path   install_lib  = form(path, "%o/lib",  install);
    path   install_bin  = form(path, "%o/app",  install);
    return 0;
}

i32 tapestry_build(tapestry a) {
    int  error_code = 0;
    bool debug = is_dbg(cstring(a->name));

    cd(a->project_path);
    AType type = isa(a->build_path);
    make_dir(a->build_path);
    path   build_lib    = form(path, "%o/lib",  a->build_path);
    path   build_app    = form(path, "%o/app",  a->build_path);
    path   build_test   = form(path, "%o/test", a->build_path);
    cstr   CC           = getenv("CC");       if (!CC)       CC       = "clang";
    cstr   CXX          = getenv("CXX");      if (!CXX)      CXX      = "clang++";
    cstr   CFLAGS       = getenv("CFLAGS");   if (!CFLAGS)   CFLAGS   = "";
    cstr   CXXFLAGS     = getenv("CXXFLAGS"); if (!CXXFLAGS) CXXFLAGS = "";
    string name         = filename(a->project_path);
    string n2           = copy(name);
    bool   cpp          = false;
    cstr   compilers[2] = { CC, CXX };
    a->lib_targets      = array();
    a->bin_targets      = array();

    struct directive {
        array list;
        cstr  dir;
        path  build_dir;
    } directives[3] = {
        { a->lib,  "lib",  build_lib  },
        { a->app,  "app",  build_app  },
        { a->test, "test", build_test }
    };
    for (int i = 0; i < sizeof(directives) / sizeof(struct directive); i++) {
        struct directive* flags = &directives[i];
        bool  is_lib    = strcmp(flags->dir, "lib") == 0;
        path  dir       = form(path, "%o/%s", a->project_path, flags->dir);
        path  lib_src   = form(path, "%o/%s", a->project_path, "lib");
        path  build_dir = form(path, "%o/%s", a->build_path, flags->dir);
        make_dir(flags->build_dir);
        if (!dir_exists("%o", dir)) continue;
        cd(flags->build_dir);

        bool is_app  = (i == 1);

        array c      = ls(dir, string(".c"),   false); // returns absolute paths
        array cc     = ls(dir, string(".cc"),  false);
        array src    = array(32);
        cpp         |= len(cc) > 0;
        array obj    = array(64);
        array obj_c  = array(64);
        array obj_cc = array(64);
        path  href   = form(path, "%o/%o", dir, name);
        array files  = ls(dir, null, false);
        i64   htime  = 0;

        if (!file_exists("%o", href))
             href = form(path, "%o/%o.h", dir, name);
    
        /// get latest header modified (including project header)
        each (files, path, f) {
            string e = ext(f);
            string s = stem(f);
            if (cmp(s, "h") == 0 || (len(e) == 0 && cmp(s, name->chars) == 0)) {
                i64 mt = modified_time(f);
                if (mt > htime)
                    htime = mt;
            }
        }
        string cflags = string(alloc, 64);
        string cxxflags = string(alloc, 64);
        each(flags->list, flag, fl) {
            if (fl->is_cflag) {
                concat(cflags, cast(string, fl));
                append(cflags, " ");
            }
        }

        /// compile C with cflags
        struct lang {
            array source;
            array objs;
            cstr  compiler;
            cstr  std;
        } langs[2] = {
            {c,  obj_c,  compilers[0], "gnu11"},
            {cc, obj_cc, compilers[1], "c++17"}
        };
        i64 latest_o = 0;
        for (int l = 0; l < 2; l++) {
            struct lang* lang = &langs[l];
            if (!lang->std) continue;
            string compiler = form(string, "%s -g2 -c -std=%s %s %o -I%o -I%o -I%o",
                lang->compiler, lang->std,
                l == 0 ? CFLAGS : CXXFLAGS, /// CFLAGS from env come first
                l == 0 ? cflags : cxxflags, /// ... then project-based flags
                dir, lib_src, include); /// finally, an explicit -I of our directive
            
            /// for each source file, make objects file names, 
            /// and compile them if they are modified prior to source
            each(lang->source, path, src) {
                string module = filename(src);
                string module_name = stem(src);
                string o_file = form(string, "%o.o",    module);
                path   o_path = form(path,   "%o/%o.o", a->build_path, o_file);
                latest_o = max(latest_o, modified_time(o_path));
                push(lang->objs, o_path);
                push(obj, o_path);
                print("adding obj: %o", o_path);

                /// recompile if newer / includes differ
                i64 mtime = modified_time(src);
                if (mtime > modified_time(o_path) || (htime && htime > mtime)) {
                    string cmd = form(string, "%o -DMODULE=\"\\\"%o\\\"\" %o -o %o", compiler, module_name, src, o_path);
                    print("compile: %o", cmd);
                    verify(system(cstring(cmd)) == 0, "compilation");
                    latest_o = max(latest_o, modified_time(o_path));
                }
            }
        }

        // link
        string lflags = string(alloc, 64);
        concat(lflags, form(string, "-L%o/lib ", a->build_path));
        concat(lflags, form(string, "-L%o/lib ", install));
        each(flags->list, flag, fl) {
            if (!fl->is_cflag) {
                concat(lflags, cast(string, fl));
                append(lflags, " ");
            }
        }

        if (is_lib) {
            path output_lib = form(path, "%o/lib/lib%o.so", a->build_path, n2);
            if (modified_time(output_lib) < latest_o) {
                string cmd = form(string, "%s -shared %o %o -o %o",
                    cpp ? CXX : CC,
                    lflags, obj, output_lib);
                print("linking (lib): %o", cmd);
                verify (system(cstring(cmd)) == 0, "lib");
                path lib = form(path, "%o/lib%o.so", flags->build_dir, n2);
                push(a->lib_targets, lib);
            }
        } else {
            each (obj_c, path, obj) {
                string module_name = stem(obj);
                path output = form(path, "%o/%s/%o", a->build_path, flags->dir, module_name);
                if (modified_time(output) < modified_time(obj)) {
                    string cmd = form(string, "%s %o %o -o %o",
                        CC, lflags, obj, output);
                    char cwd[1024];
                    getcwd(cwd, sizeof(cwd));
                    print("linking (c): %o", cmd);
                    verify (system(cstring(cmd)) == 0, "app");
                }
                if (is_app)
                    push(a->bin_targets, output);
            }
            each (obj_cc, path, obj) {
                string module_name = stem(obj);
                path output_o = form(path, "%o/%s/%o", a->build_path, flags->dir, module_name);
                if (modified_time(output_o) < modified_time(obj)) {
                    string cmd = form(string, "%s %o %o %o -o %o",
                        CXX, lflags, obj, output_o);
                    print("linking (cc): %o", cmd);
                    verify (system(cstring(cmd)) == 0, "app");
                }
            }
            // run test here, to verify prior to install; will need updated PATH so they may run the apps we built just prior to test
        }
    }
    return error_code;
}

/// initialization path
none tapestry_init(tapestry a) {
    a->name = filename(a->project_path);
}

int main(int argc, cstr argv[]) {
    A_start();
    print("running tapestry");
    cstr  SRC             = getenv("SRC");
    cstr  TAPESTRY        = getenv("TAPESTRY");
    cstr  DBG             = getenv("DBG");
    path  default_path    = form  (path, "%s", ".");
    path  default_src     = form  (path, "%s", SRC      ? SRC      : "");
    path  default_install = form  (path, "%s", TAPESTRY ? TAPESTRY : ".");
    map   args            = A_args(argc, argv,
        "path",    default_path,
        "install", default_install,
        "src",     default_src, null);

    path  path_unrel      = get (args, string("path"));
    path  src_unrel       = get (args, string("src"));
    path  install_unrel   = get (args, string("install"));

    dbg      = DBG ? string(DBG) : string("");
    src      = absolute(src_unrel);
    install  = absolute(install_unrel);
    print("install is set to %o", install);
    
    include  = form(path, "%o/include", install);
    path loc = absolute(path_unrel);
    path f   = null;

    make_dir(form(path, "%o",           install));
    make_dir(form(path, "%o/lib",       install));
    make_dir(form(path, "%o/bin",       install));
    make_dir(form(path, "%o/tokens",    install));

    if (len(loc))
        cd(loc);

    if (dir_exists("%o", loc) && file_exists("%o/build", loc))
        f  = form(path, "%o/build", loc);
    else if (file_exists("%o", loc))
        f  = form(path, "%o", loc);
    else
        fault("tapestry: cannot build from path[ %o ]", loc);
    
    path     af = absolute(f);
    tapestry t  = tapestry(af);
    
    i32 import_code  = tapestry_import (t); if (import_code  != 0) return import_code;
    i32 build_code   = tapestry_build  (t); if (build_code   != 0) return build_code;
    i32 install_code = tapestry_install(t); if (install_code != 0) return install_code;
    print("success");
    return 0;
}

define_class(import)
define_class(tapestry)
define_class(flag)