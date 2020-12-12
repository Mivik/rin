
add_library(rin
	codegen.cpp
	core_context.cpp
	lexer.cpp
	parser.cpp
	token.cpp
	type.cpp
	value.cpp
)

target_link_libraries(rin ${llvm_libs})
