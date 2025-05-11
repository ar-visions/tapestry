#include <A-public>
#undef link
#include <tapestry-intern>
#include <tapestry>
#include <tapestry-init>
#include <tapestry-methods>
#include <A-init>
#include <A-methods>
#include <signal.h>
#include <unistd.h>

void on_signal(int sig) {
    pid_t pid = A_last_pid();
    printf("\ntapestry: signal %i\n", sig);
    if (pid > 0)
        killpg(pid, SIGTERM);
    exit(1);
} 

int main(int argc, cstr argv[]) {
    A_start();

    signal(SIGINT,  on_signal);  // ctrl+c
    signal(SIGTERM, on_signal);  // kill
    signal(SIGHUP,  on_signal);  // terminal closed

    setenv("VERBOSE", "1", true);
    cstr  _TAPESTRY       = getenv("TAPESTRY");
    cstr  _DBG            = getenv("DBG");
    path  default_path    = form  (path, "%s", ".");
    path  default_install = form  (path, "%s", _TAPESTRY ? _TAPESTRY : ".");
    map   args            = A_args(argc, argv,
        "path",    default_path,
        "install", default_install,
        null);

    path  path_unrel      = get (args, string("path"));
    path  install_unrel   = get (args, string("install"));

    string dbg      = _DBG ? string(_DBG) : string("");
    path   install  = absolute(install_unrel);
    path   src      = parent(install);

    print("install is set to %o, and src for sym-linking is %o", install, src);
    
    path   loc      = absolute(path_unrel);
    path   f        = null;

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
    map      m  = map_of(
        "path",    af,
        "dbg",     _DBG ? string(_DBG) : string(""),
        "install", absolute(install_unrel),
        "src",     parent(install));
    tapestry t  = tapestry(m);
    
  //i32 import_code  = import (t);       if (import_code  != 0) return import_code;
    i32 build_code   = build  (t, null); if (build_code   != 0) return build_code;
    i32 install_code = install(t);       if (install_code != 0) return install_code;
    print("success");
    return 0;
}
