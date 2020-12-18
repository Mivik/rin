
add_library(rin SHARED
	ast.cpp
	codegen.cpp
	context.cpp
	core_context.cpp
	lexer.cpp
	parser.cpp
	token.cpp
	type.cpp
	value.cpp
)

target_link_libraries(rin ${llvm_libs})
