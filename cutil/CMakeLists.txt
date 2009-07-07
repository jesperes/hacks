project(cutil)
cmake_minimum_required(VERSION 2.6)

add_library(cutil STATIC
  src/strbuf.c
  include/strbuf.h
  )

include_directories(include)

add_executable(cutiltest runtests.c)
target_link_libraries(cutiltest cutil)