cmake_minimum_required(VERSION 3.12)

project(
  paqFe-RTL
  VERSION 0.0.1
  DESCRIPTION "A FPGA implementation of paqFe"
  HOMEPAGE_URL "https://github.com/Jomit626/paqFe-RTL"
  )

set(BUILD_PAQFE ON CACHE INTERNAL "build software binary" FORCE)
add_subdirectory(paqFe)

include(verify/CMakeLists.txt)
include(driver/CMakeLists.txt)