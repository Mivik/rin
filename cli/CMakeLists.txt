
FIND_PACKAGE(Boost 1.74 COMPONENTS program_options REQUIRED)
INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})

aux_source_directory(. RIN_CLI_SOURCE_FILES)

add_executable(rin_cli ${RIN_CLI_SOURCE_FILES})

# TODO rin->rin_core, rin_cli->rin?
target_link_libraries(rin_cli rin ${Boost_LIBRARIES})
