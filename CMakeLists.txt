cmake_minimum_required(VERSION 3.10)

project(fys4460)
enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()


set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})
add_subdirectory("fortperc")

set(generalflags "-Wall -Wextra")
set(speedflags "-Ofast -fopenmp -fopt-info")
set(debugflags "-O0 -g -fcheck=all")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${generalflags}")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE} ${speedflags}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG} ${debugflags}")

set(mylibraries utilities percolation lapack hk randomwalk)

function(addexewithlibs name)
    add_executable(${name} ${CMAKE_SOURCE_DIR}/src/${name}.f90)
    target_link_libraries(${name} ${mylibraries})
endfunction()

foreach(exe k r2 verification stuff)
    addexewithlibs(${exe})
endforeach()
