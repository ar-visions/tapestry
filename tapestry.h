#ifndef TAPESTRY
#define TAPESTRY
//#include <A>

forward(tapestry)

#define import_schema(X,Y,...) \
    i_prop(X,Y, public, tapestry, tapestry) \
    i_prop(X,Y, public, string, name) \
    i_prop(X,Y, public, string, uri) \
    i_prop(X,Y, public, string, commit) \
    i_prop(X,Y, public, path,   import_path) \
    i_prop(X,Y, public, path,   build_path) \
    i_method(X,Y, public, bool, make) \
    i_prop(X,Y, public, map,    environment) \
    i_prop(X,Y, public, array, config,   of, string) \
    i_prop(X,Y, public, array, commands, of, string) \
    i_override(X,Y, cast, string)
#define import_intern(X,Y,...) import_schema(X,Y,__VA_ARGS__)
declare_class(import)

#define flag_schema(X,Y,...) \
    i_prop(X,Y, public, string, name) \
    i_prop(X,Y, public, bool,   is_cflag) \
    i_prop(X,Y, public, bool,   is_static) \
    i_override(X,Y, cast, string)
#define flag_intern(X,Y,...) flag_schema(X,Y,__VA_ARGS__)
declare_class(flag)

#define tapestry_schema(X,Y,...) \
    i_ctr(X,Y,  public, path) \
    i_prop(X,Y, public, string, name) \
    i_prop(X,Y, public, path,  project_path) \
    i_prop(X,Y, public, path,  build_path) \
    i_prop(X,Y, public, array, imports, of, import) \
    i_prop(X,Y, intern, array, lib_targets) \
    i_prop(X,Y, intern, array, bin_targets) \
    i_prop(X,Y, public, array, lib,  of, flag) \
    i_prop(X,Y, public, array, app,  of, flag) \
    i_prop(X,Y, public, array, test, of, flag) \
    i_method(X,Y, public, i32, build) \
    i_override(X,Y, method, init)
#define tapestry_intern(X,Y,...) tapestry_schema(X,Y,__VA_ARGS__)
declare_class(tapestry)

#endif
