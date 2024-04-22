cmake_minimum_required(VERSION 2.8.12)
project(tapestry)

# select all of mx into tapestry; we'll add functionality as C99 interface functions that the scripts will use
# theres nothing complex in the modules, so the source can simply be selected
file(GLOB_RECURSE mx_src "./deps/mx/**/*.cpp")

add_library               (tapestry_library   SHARED ${mx_src})
add_executable            (tapestry_generator tapestry/tapestry.cpp)

set_target_properties     (tapestry_library   PROPERTIES OUTPUT_NAME tapestry)
set_target_properties     (tapestry_generator PROPERTIES OUTPUT_NAME tapestry)
target_include_directories(tapestry_library   PRIVATE . ./deps/mx)
target_include_directories(tapestry_generator PRIVATE . ./deps/mx)