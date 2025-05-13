#include <A-public>
#undef link
#include <tapestry-intern>
#include <tapestry>
#include <tapestry-methods>
#include <tapestry-init>
#include <A-init>
#include <A-methods>
#include <sys/stat.h>
#include <utime.h>

static i64 ancestor_mod = 0;

#if defined(__x86_64__) || defined(_M_X64)
static symbol arch = "x86_64";
#elif defined(__i386__) || defined(_M_IX86)
static symbol arch = "x86";
#elif defined(__aarch64__) || defined(_M_ARM64)
static symbol arch = "arm64";
#elif defined(__arm__) || defined(_M_ARM)
static symbol arch = "arm32";
#endif

#if defined(__linux__)
static symbol lib_pre  = "lib"; static symbol lib_ext  = ".so";     static symbol app_ext  = "";        static symbol platform = "linux";
#elif defined(_WIN32)
static symbol lib_pre  = "";    static symbol lib_ext  = ".dll";    static symbol app_ext  = ".exe";    static symbol platform = "windows";
#elif defined(__APPLE__)
static symbol lib_pre  = "lib"; static symbol lib_ext  = ".dylib";  static symbol app_ext  = "";        static symbol platform = "darwin";
#endif

bool is_debug(tapestry t, string name) {
    string f = f(string, ",%o,", t->dbg);
    string s = f(string, ",%o,", name);
    return strstr(f->chars, s->chars) != null;
}

none sync_tokens(tapestry t, path build_path, string name) {
    path t0 = form(path, "%o/tapestry-token", build_path);
    path t1 = form(path, "%o/tokens/%o", t->install, name);
    struct stat build_token, installed_token;
    /// create token pair (build & install) to indicate no errors during config/build/install
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
}

none import_init(import im) {
    path   cwd     = path_cwd(4096);
    path   install = im->tapestry->install;
    path   src     = im->tapestry->src;
    bool   debug      = is_debug(im->tapestry, im->name);
    symbol build_type = debug ? "debug" : "release";
    path checkout     = f(path, "%o/checkout", install);
    im->import_path   = f(path, "%o/%o", checkout, im->name);
    im->build_path    = f(path, "%o/%s", im->import_path, build_type);
    if (!dir_exists("%o/%o", checkout, im->name)) {
        cd(checkout);
        if (A_len(src) && dir_exists("%o/%o", src, im->name)) {
            verify (exec("ln -s %o/%o %o/%o",
                src, im->name, checkout, im->name) == 0, "symlink");
        } else {
            exec("rm -rf %o", im->name);
            int clone = exec("git clone %o %o --no-checkout && cd %o && git checkout %o && cd ..",
                im->uri, im->name, im->name, im->commit);
            verify (clone == 0, "git clone");
            if (file_exists("%o/diff/%o.patch", im->tapestry->project_path, im->name)) {
                cd(im->import_path);
                verify(exec("git apply %o/diff/%o.patch", im->tapestry->project_path, im->name) == 0, "patch");
                cd(checkout);
            }
        }
    }
    string n = im->name;
    i32* c = null, *b = null, *i = null;
    if (file_exists("%o/build", im->import_path)) {
        path af_remote = form(path, "%o/build", im->import_path);
        map m_copy = copy(im->tapestry->m);
        set(m_copy, string("parent"), im->tapestry);
        set(m_copy, string("path"), af_remote);
        tapestry t_remote = tapestry(m_copy);
        im->exports = hold(t_remote->exports);
        i64 mod = modified_time(f(path, "%o/tokens/%o", im->tapestry->install, im->name));
        if (mod && (!ancestor_mod || ancestor_mod < mod))
             ancestor_mod = mod;
        drop(t_remote);
    }
    make_dir(im->build_path);
    cd(im->build_path);
    make(im);
    cd(im->build_path);
    path   cwd_after = path_cwd(4096);
    each(im->commands, string, cmd) {
        print("> %o: command: %o", im->name, cmd);
        verify(exec("%o", evaluate(cmd, im->tapestry->environment)) == 0, "expected successful command post-install");
    }
    each(im->always, string, cmd) {
        print("! %o: command: %o", im->name, cmd);
        verify(exec("%o", evaluate(cmd, im->tapestry->environment)) == 0, "expected successful ! inline command");
    }
    sync_tokens(im->tapestry, im->build_path, im->name);
    cd(cwd);
}

string import_cast_string(import a) {
    string config = string(alloc, 128);
    each (a->config, string, conf) {
        if (starts_with(conf, "-l"))
            continue;
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

none add_flag(tapestry a, array list, line l, map environment) {
    each (l->text, string, w) {
        string directive = get(environment, string("DIRECTIVE"));
        string t = evaluate(w, environment); // PROJECT is missing null char
        if (len(t)) {
            //print("%o: %o", top_directive, t);
            bool is_lib    = t->chars[0] == '-' && t->chars[1] == 'l';
            bool is_cflag  = t->chars[0] == '-';
            bool is_static = t->chars[0] == '@';
            push(list, flag(name,
                    is_static ? mid(t, 1, len(t) - 1) : 
                    is_lib    ? mid(t, 2, len(t) - 2) : t,
                is_static, is_static, is_cflag, is_cflag, is_lib, is_lib));
        }
    }
}

none tapestry_with_map(tapestry a, map m) {
    tapestry parent       = get(m, string("parent"));
    path af               = get(m, string("path"));
    a->m                  = hold(m);
    a->dbg                = get(m, string("dbg"));
    a->install            = get(m, string("install"));
    a->src                = get(m, string("src"));
    a->imports            = array(64);
    a->project_path       = directory(af);
    a->name               = filename(a->project_path);
    cstr build_dir        = is_debug(a, a->name) ? "debug" : "release";
    a->build_path         = form(path, "%o/%s", a->project_path, build_dir);
    a->exports            = array(64);
    a->interns            = array(64);
    a->environment        = map();
    array  lines          = read(af, typeid(array));
    import im             = null;
    string last_arch      = null;
    string last_platform  = null;
    string last_directive = null;
    string top_directive  = null;
    bool   is_import      = true;
    string s_imports      = string(alloc, 64);
    path   project_lib    = form(path, "%o/lib",  a->project_path);
    a->has_lib            = dir_exists("%o", project_lib); /// and contains source?

    /// for each line, process by indentation level, and evaluate tokens
    /// when exports are evaluated by a parent, the environment but reflect their own
    set(a->environment, string("PROJECT"),      a->name);
    set(a->environment, string("PROJECT_PATH"), a->project_path);
    set(a->environment, string("BUILD_PATH"),   a->build_path);
    set(a->environment, string("IMPORTS"),      s_imports);
     
    map environment = parent ? parent->environment : a->environment;

    each(lines, line, l) {
        string first = get(l->text, 0);
        i32    flen  = len(first);

        /// handle import/lib/app/test (must be indent 0)
        if (l->indent == 0) {
            verify(first->chars[flen - 1] == ':', "expected base-directive and ':'");
            last_directive = mid(first, 0, flen - 1);
            verify(cmp(last_directive, "import") == 0 ||
                   cmp(last_directive, "export") == 0, "expected import or export directive");
            is_import = cmp(last_directive, "import") == 0;
            top_directive = copy(last_directive);
            set(environment, string("DIRECTIVE"),
                is_import ? string("lib") : 
                (parent ? parent->has_lib : a->has_lib) ? string("lib") : // must be based on parent if set
                            string("app"));

        } else if (first->chars[flen - 1] == ':') {
            /// independent of indent, up to user in how to construct
            string n = mid(first, 0, flen - 1);
            if (cmp(n, "linux") == 0 || cmp(n, "windows") == 0 || cmp(n, "darwin") == 0)
                last_platform = n;
            else
                last_arch = n;
        } else if (l->indent == 1) {
            if (!parent) {
                string text0 = get(l->text, 0);
                string name   = evaluate(text0, environment);
                if (name->chars[0] == '-') {
                    /// we want our main target (lib or app, if isolated) to get these cflag or libs
                    /// its the same as 'export' section, but not exported
                    add_flag(a, a->interns, l, environment);
                } else {
                    verify(is_import, "expected import");
                    string uri    = evaluate(get(l->text, 1), environment);
                    string commit = evaluate(get(l->text, 2), environment);
                    if (len(s_imports))
                        append(s_imports, " ");
                    concat(s_imports, name);
                    im = allocate(import,
                        tapestry, a,
                        name,   name,
                        uri,    uri,
                        commit, commit,
                        environment, map(),
                        config, array(64), commands, array(16), always, array(16)); // we must initialize this
                    // we need to create this complete!
                    push(a->imports, im);
                }

                last_platform = null;
                last_arch = null;
            } else if (!is_import) {
                add_flag(a, a->exports, l, environment);
            }
        } else if (!parent) {
            if ((!last_platform || cmp(last_platform, platform) == 0) &&
                (!last_arch     || cmp(last_arch,     arch)     == 0)) {
                bool use_command = cmp(first, ">") == 0;
                if (use_command || cmp(first, "!") == 0) {
                    // combine tokens into singular string
                    string cmd = string(alloc, 64);
                    each(l->text, string, w) {
                        if (first == w) continue;
                        if (len(cmd))
                            append(cmd, " ");
                        concat(cmd, w);
                    }
                    push(use_command ? im->commands : im->always, cmd);
                } else {
                    each(l->text, string, w) {
                        /// if its VAR=VAL then this is not 'config' but rather environment
                        char f = w->chars[0];
                        cstr e = strstr(w->chars, "=");
                        if (isalpha(f) && (f >= 'A' && f <= 'Z') && e) {
                            char name[128];
                            sz   ln = (sz)(e - w->chars);
                            verify(ln < 128, "invalid data");
                            memcpy(name, w->chars, ln);
                            name[ln] = 0;
                            char value[128];
                            sz value_ln = len(w) - ln - 1;
                            memcpy(value, &w->chars[ln + 1], value_ln);
                            string n   = trim(string(name));
                            string val = evaluate(string(value), environment);
                            set(im->environment, n, val);
                        } else {
                            string e = evaluate(w, environment);
                            push(im->config, e);
                        }
                    }
                }
            }
        }
    }

    if (!parent) {
        /// this will perform actual import (now that the data is set)
        each(a->imports, import, im) {
            A_initialize(im);
        }
    }
}

string import_cmake_location(import im) {
    int index = 0;
    each (im->config, string, conf) {
        if (starts_with(conf, "-S")) {
            return form(string, "%o %o", conf, im->config->elements[index + 1]);
        }
        index++;
    }
    return null;
}

/// handle autoconfig (if present) and resultant / static Makefile
bool import_make(import im) {
    tapestry t = im->tapestry;
    path install = t->install;
    i64 conf_status = INT64_MIN;
    bool debug      = is_debug(t, im->name);
    path t0 = form(path, "tapestry-token");
    path t1 = form(path, "%o/tokens/%o", install, im->name);
    path checkout = form(path, "%o/checkout", install);

    make_dir(im->build_path);
    cd(im->build_path);
    string env = serialize_environment(im->environment, false);
    
    if (file_exists("%o", t0) && file_exists("%o", t1)) {
        /// get modified date on token, compare it to one in install dir
        i64 token0 = modified_time(t0);
        i64 token1 = modified_time(t1);

        if (token0 && token1) {
            conf_status = abs((i64)(token0 - token1));
            if (conf_status < 1000)
                conf_status = 0;
        }
        if (conf_status == 0) {
            i64 latest = 0;
            path latest_f = latest_modified(im->import_path, &latest);
            if (latest > token0 || ancestor_mod > token0)
                conf_status = -1;
        }
    }

    if (conf_status == 0) {
        clear(im->commands);
        print("%22o: build-cache", im->name);
        return true;
    }

    string cmake_conf = cmake_location(im);
    string args = cast(string, im);
    cstr debug_r = debug ? "debug" : "release";
    setenv("BUILD_CONFIG", args->chars, 1);
    setenv("BUILD_TYPE", debug_r, 1);

    if (file_exists("../Cargo.toml")) {
        // todo: copy bin/lib after
        verify(exec("cargo build --%s --manifest-path ../Cargo.toml --target-dir .",
            debug ? "debug" : "release") == 0, "rust compilation");
    } else if (cmake_conf || file_exists("../CMakeLists.txt")) {
        /// configure
        if (!file_exists("CMakeCache.txt")) {
            cstr build = debug ? "Debug" : "Release";
            int  iconf = exec(
                "cmake -B . -S .. -DCMAKE_INSTALL_PREFIX=%o -DCMAKE_BUILD_TYPE=%s %o", install, build, im);
            verify(iconf == 0, "%o: configure failed", im->name);
        }
        /// build & install
        int    icmd = exec("%o cmake --build . -j16", env);
        int   iinst = exec("%o cmake --install .",   env);

    } else {
        cstr Makefile = "Makefile";
        /// build for A-type projects
        if (file_exists("../%s", Makefile) && file_exists("../build", Makefile)) {
            cd(im->import_path);
            int imake = exec("%o make", env);
            verify(imake == 0, "make");
            cd(im->build_path);
        } else if (!file_exists("Makefile")) {
            cd(im->import_path);
            /// build for automake projects
            if (file_exists("./configure.ac") || file_exists("./configure") || file_exists("./config")) {
                /// generate configuration scripts if available
                if (!file_exists("./configure") && file_exists("./configure.ac")) {
                    verify(exec("autoupdate .")    == 0, "autoupdate");
                    verify(exec("pwd") == 0, "autoreconf");
                    verify(exec("autoreconf -i .") == 0, "autoreconf");
                }
                /// prefer our pre/generated script configure, fallback to config
                cstr configure = file_exists("./configure") ? "./configure" : "./config";
                if (file_exists("%s", configure)) {
                    verify(exec("%o %s%s --prefix=%o %o",
                        env,
                        configure,
                        debug ? " --enable-debug" : "",
                        install,
                        im) == 0, configure);
                }
            }
            if (file_exists("%s", Makefile)) {
                path cwd = path_cwd(2048);
                verify(exec("%o make -f %s install", env, Makefile) == 0, "make");
            }
        }
    }
    return true;
}

static bool is_dbg(cstr name) {
    cstr dbg = getenv("DBG");
    return dbg ? strstr(dbg, name) != 0 : false;
}

static array headers(path dir) {
    array all = ls(dir, null, false);
    array res = array();
    each (all, path, f) {
        string e = ext(f);
        if (len(e) == 0 || cmp(e, ".h") == 0)
            push(res, f);
    }
    return res;
}

static int filename_index(array files, path f) {
    string fname = filename(f);
    int    index = 0;
    each(files, path, p) {
        string n = filename(p);
        if (compare(n, fname) == 0)
            return index;
        index++;
    }
    return -1;
}

/// install headers, then overlay built headers; then install libs and app targets
i32 tapestry_install(tapestry a) {
    path   install      = a->install;
    path   install_inc  = form(path, "%o/include", install);
    path   install_lib  = form(path, "%o/lib",     install);
    path   install_app  = form(path, "%o/bin",     install);
    path   project_lib = form(path, "%o/lib", a->project_path);
    path   build_lib   = form(path, "%o/lib", a->build_path);
    array  project_h   = headers(project_lib);
    array  build_h     = headers(build_lib);

    each(project_h, path, f)
        if (filename_index(build_h, f) < 0)
            exec("rsync -a %o %o/", f, install_inc);

    each (build_h, path, f)
        if (!eq(filename(f), "import"))
            exec("rsync -a %o %o/", f, install_inc);

    each(a->lib_targets, path, lib)
        exec("rsync -a %o %o/%o", lib, install_lib, filename(lib));

    each(a->app_targets, path, app)
        exec("rsync -a %o %o/%o", app, install_app, filename(app));

    return 0;
}

none cflags_libs(tapestry a, string* cflags, string* libs) {
    *libs   = string(alloc, 64);
    *cflags = string(alloc, 64);

    array lists[2] = { a->interns, a->exports };
    for (int i = 0; i < 2; i++)
        each (lists[i], flag, fl) {
            print("%o: %o", a->name, fl->name);
            if (fl->is_cflag) {
                concat(*cflags, cast(string, fl));
                append(*cflags, " ");
            } else if (fl->is_lib) {
                concat(*libs, form(string, "-l%o", fl->name));
                append(*libs, " ");
            }
        }
    
    each (a->imports, import, im) {
        if (im->exports)
            each (im->exports, flag, fl) {
                if (fl->is_cflag) {
                    concat(*cflags, cast(string, fl));
                    append(*cflags, " ");
                } else if (fl->is_lib) {
                    concat(*libs, form(string, "-l%o", fl->name));
                    append(*libs, " ");
                }
            }
        
        bool has_lib = false;
        each (im->config, string, conf) {
            if (starts_with(conf, "-l")) {
                concat(*libs, conf);
                append(*libs, " ");
                has_lib = true;
            }
        }
        if (!has_lib) {
            concat(*libs, form(string, "-l%o", im->name));
            append(*libs, " ");
        }
    }
}

// build with optional bc path; if no bc path we use the project file system
i32 tapestry_build(tapestry a, path bc) {
    int  error_code = 0;
    bool debug = is_dbg(cstring(a->name));

    if (bc) {
        // simplified process for .bc case
        string name = stem(bc);
        verify(exec("%o/bin/llc -filetype=obj %o.ll -o %o.o -relocation-model=pic",
            a->install, name, name) == 0,
                ".ll -> .o compilation failed");
        string libs, cflags;
        cflags_libs(a, &cflags, &libs); // fetches from all exported data
        verify(exec("%o/bin/clang %o.o -o %o -L %o/lib %o %o",
            a->install, name, name, a->install, libs, cflags) == 0,
                "link failed");
        return 0;
    }

    cd(a->project_path);
    AType type = isa(a->build_path);
    make_dir(a->build_path);

    path   project_lib  = form  (path, "%o/lib",  a->project_path);
    path   build_lib    = form  (path, "%o/lib",  a->build_path);
    path   build_app    = form  (path, "%o/app",  a->build_path);
    path   build_test   = form  (path, "%o/test", a->build_path);
    cstr   CC           = getenv("CC");       if (!CC)       CC       = "clang";
    cstr   CXX          = getenv("CXX");      if (!CXX)      CXX      = "clang++";
    cstr   CFLAGS       = getenv("CFLAGS");   if (!CFLAGS)   CFLAGS   = "";
    cstr   CXXFLAGS     = getenv("CXXFLAGS"); if (!CXXFLAGS) CXXFLAGS = "";
    string name         = filename(a->project_path);
    string n2           = copy(name);
    bool   cpp          = false;
    cstr   compilers[2] = { CC, CXX };
    path   include      = form(path, "%o/include", a->install);
    a->lib_targets      = array();
    a->app_targets      = array();
    bool has_lib        = dir_exists("%o", project_lib);

    struct dir {
        cstr  dir;
        cstr  output_dir;
        path  build_dir;
    } dirs[3] = {
        { "lib",  "lib",  build_lib  },
        { "app",  "bin",  build_app  },
        { "test", "test", build_test } // not allowed if no lib
    };

    for (int i = 0; i < sizeof(dirs) / sizeof(struct dir); i++) {
        struct dir* flags = &dirs[i];
        bool  is_lib    = strcmp(flags->dir, "lib") == 0;
        path  dir       = form(path, "%o/%s", a->project_path, flags->dir);
        path  lib_src   = form(path, "%o/%s", a->project_path, "lib");
        path  build_dir = form(path, "%o/%s", a->build_path, flags->output_dir);
        path  build_dir2 = form(path, "%o/%o", a->build_path, flags->build_dir);
        make_dir(flags->build_dir);
        make_dir(build_dir);
        make_dir(build_dir2);
        if (!dir_exists("%o", dir)) continue;

        string base_cflags = string(""), base_libs = string("");
        /// if its a stand-alone app or has a lib, it should get all of the exports
        /// also lib gets everything (i == 0)
        if (has_lib && (i == 0) || !has_lib) {
            cflags_libs(a, &base_cflags, &base_libs);
        }
        /// app and test depend on its lib
        if (has_lib && i != 0)
            base_libs = form(string, "%o -l%o", base_libs, name);

        cd(flags->build_dir);

        bool is_app  = (i == 1);

        array c      = ls(dir, string(".c"),   false); // returns absolute paths
        array cc     = ls(dir, string(".cc"),  false);
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
            string compiler = form(string, "%s -c %s-std=%s %s %o %o -I%o -I%o -I%o -I%o",
                lang->compiler, debug ? "-g2 " : "", lang->std,
                l == 0 ? CFLAGS : CFLAGS, /// CFLAGS from env come first
                base_cflags,
                l == 0 ? cflags : cflags, /// ... then project-based flags
                (has_lib && i == 2) ? build_lib : flags->build_dir,
                dir, lib_src, include); /// finally, an explicit -I of our directive
            
            /// for each source file, make objects file names, and compile if changed
            each(lang->source, path, src) {
                string module = filename(src);
                string module_name = stem(src);
                string o_file = form(string, "%o.o",  module);
                path   o_path = form(path,   "%o/%o", a->build_path, o_file);
                i64    o_modified  = modified_time(o_path);
                latest_o = max(latest_o, o_modified);
                push(lang->objs, o_path);
                push(obj, o_path);

                /// recompile if newer / includes differ
                i64 mtime = modified_time(src);
                bool source_newer  = (mtime > o_modified) || (a->ancestor_mod && (a->ancestor_mod < mtime)); // || ; /// | project_rebuild (based on newest project we depend on)
                bool header_change = htime && htime > modified_time(o_path);
                if (source_newer || header_change) {
                    int compile = exec("%o -DMODULE=\"\\\"%o\\\"\" %o -o %o", compiler, module_name, src, o_path);
                    verify(compile == 0, "compilation");
                    latest_o = max(latest_o, modified_time(o_path));
                }
            }
        }

        // link
        string lflags = string(alloc, 64);
        //concat(lflags, form(string, "-L%o/lib ", a->build_path));
        concat(lflags, form(string, "-L%o/lib -Wl,-rpath,%o/lib", a->install, a->install));

        if (is_lib) {
            path output_lib = form(path, "%o/lib/%s%o%s", a->build_path, lib_pre, n2, lib_ext);
            path install_lib = form(path, "%o/lib/%s%o%s", a->install, lib_pre, n2, lib_ext);
            if (!latest_o || (modified_time(install_lib) < latest_o)) {
                verify (exec("%s -shared %o %o %o -o %o",
                    cpp ? CXX : CC,
                    lflags, base_libs, obj, output_lib) == 0, "linking");
            }
            // install lib right away
            exec("rsync -a %o %o", output_lib, install_lib);
            //push(a->lib_targets, output_lib);
        } else {
            each (obj_c, path, obj) {
                string module_name = stem(obj);
                path output = form(path, "%o/%s/%o%s", a->build_path, flags->output_dir, module_name, app_ext);
                if (modified_time(output) < modified_time(obj)) {
                    verify (exec("%s %o %o %o -o %o",
                        CC, lflags, base_libs, obj, output) == 0, "linking");
                }
                if (is_app)
                    push(a->app_targets, output);
            }
            each (obj_cc, path, obj) {
                string module_name = stem(obj);
                path output_o = form(path, "%o/%s/%o", a->build_path, flags->output_dir, module_name);
                if (modified_time(output_o) < modified_time(obj)) {
                    verify (exec("%s %o %o %o -o %o",
                        CXX, lflags, obj, output_o) == 0, "linking");
                }
            }
            // run test here, to verify prior to install; will need updated PATH so they may run the apps we built just prior to test
        }
    }
    
    each (a->imports, import, im) {
        if (!dir_exists("%o/share", im->import_path))
            continue;
        verify (exec("rsync -a %o/share/ %o/share/%o",
            im->import_path, a->install, a->name) == 0,
                "import resources");
    }

    /// an app could perhaps initialize its own installation in a post-install -- fetching resources etc
    if (dir_exists("%o/share", a->project_path)) {
        verify (exec("rsync -a %o/share/ %o/share/%o/", a->project_path, a->install, a->name) == 0,
            "project resources");
    }

    /// now we set the token here! (we do this twice; the import layer does it 
    // (and CAN be skipped if its a tapestry project -- should be without side effect, though, since its immediately after))
    sync_tokens(a, a->build_path, a->name);

    return error_code;
}

/// initialization path
none tapestry_init(tapestry a) {
    a->name = filename(a->project_path);
}

define_class(import)
define_class(tapestry)
define_class(flag)
