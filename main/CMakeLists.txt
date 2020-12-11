
add_library(rin
	ast.cpp
	core_context.cpp
	lexer.cpp
	parser.cpp
	token.cpp
	type.cpp
)

target_link_libraries(rin ${llvm_libs})
