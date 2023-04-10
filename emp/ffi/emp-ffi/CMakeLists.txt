cmake_minimum_required(VERSION 3.0)
project(emp-ffi)
set(NAME "emp-ffi")

find_path(CMAKE_FOLDER NAMES cmake/emp-tool-config.cmake)
include(${CMAKE_FOLDER}/cmake/emp-base.cmake)

find_package(emp-tool REQUIRED)
find_package(emp-ot REQUIRED)
find_package(emp-sh2pc REQUIRED)

add_library(${NAME} SHARED emp-ffi.cpp)

target_link_libraries(${NAME} ${EMP-TOOL_LIBRARIES})

target_include_directories(${NAME} PUBLIC ${EMP-TOOL_INCLUDE_DIRS})

install(FILES emp-ffi.h DESTINATION include)
install(TARGETS ${NAME} DESTINATION lib)
