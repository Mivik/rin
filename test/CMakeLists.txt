
find_package(GTest REQUIRED)

include_directories(${GTEST_INCLUDE_DIRS})

add_executable(rin_test
	exec.cpp
	layer_map.cpp
	lexer.cpp
	main.cpp
	parser.cpp
	type.cpp
	utility.cpp
)

target_link_libraries(rin_test ${GTEST_BOTH_LIBRARIES} pthread rin)

gtest_discover_tests(rin_test)

add_custom_target(run_test COMMAND rin_test)
