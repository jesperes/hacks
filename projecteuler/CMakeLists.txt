project(euler)
cmake_minimum_required(VERSION 2.6)
if (CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_C_FLAGS "-g -O2 -Wall -Werror -std=c99")
endif()

add_library(eulerlib STATIC eulerlib.c)
add_executable(eulerlib_test eulerlib.c)

foreach(n 1 2 3 4 5 6 12 21 23 27 32 34 45 47 71 73)
  add_executable(problem${n} problem${n}.c)
  target_link_libraries(problem${n} eulerlib)
endforeach()

foreach(n 38 145)
  add_executable(problem${n} problem${n}.cpp)
  target_link_libraries(problem${n} eulerlib)
endforeach()

target_link_libraries(problem27 -lgmp)
target_link_libraries(problem71 -lgmp)
target_link_libraries(problem73 -lgmp)
