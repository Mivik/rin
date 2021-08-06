
find_package(GTest REQUIRED)

include_directories(${GTEST_INCLUDE_DIRS})

aux_source_directory(. RIN_TEST_SOURCE_FILES)

add_executable(rin_test ${RIN_TEST_SOURCE_FILES})

target_link_libraries(rin_test ${GTEST_BOTH_LIBRARIES} pthread rin)

gtest_discover_tests(rin_test)

add_custom_target(run_test COMMAND rin_test)
