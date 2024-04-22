/// mx will not be in tapestry api
#include <tapestry/tapestry.hpp>

/// select mx for internal usage in tapestry
#include <mx/mx.hpp>

int main(int argc, char **argv) {
    map def {
        { "project", str("project") }
    };
    map args { map::parse(argc, argv, def) };
    str project { args["project"] };
    
    console.log("project is {0}", { project })
    return 0;
}