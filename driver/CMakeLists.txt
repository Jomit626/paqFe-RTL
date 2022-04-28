find_package(Threads REQUIRED)

add_executable(paqfe-rtl driver/main.cpp)

target_link_libraries(paqfe-rtl PUBLIC paqFe)
target_link_libraries(paqfe-rtl PUBLIC Threads::Threads)
