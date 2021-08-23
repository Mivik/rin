
aux_source_directory(. RIN_CLI_SOURCE_FILES)

add_executable(rin_cli ${RIN_CLI_SOURCE_FILES})

target_precompile_headers(rin_cli
	PRIVATE
		llvm_stuff.h
		args.hxx
)

# TODO rin->rin_core, rin_cli->rin?
target_link_libraries(rin_cli rin)
