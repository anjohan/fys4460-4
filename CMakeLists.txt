cmake_minimum_required(VERSION 3.10)

project(fys4460-4 Fortran)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})
add_subdirectory(fortperc)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fcheck=all -Wall")
endif()

function(addexewithlibs name)
    add_executable(${name} ${CMAKE_SOURCE_DIR}/src/${name}.f90)
    target_link_libraries(${name} fortperc)
endfunction()

foreach(exe k r2 P verification stuff)
    addexewithlibs(${exe})
endforeach()
