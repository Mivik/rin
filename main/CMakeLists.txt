
aux_source_directory(. RIN_MAIN_SOURCE_FILES)

add_library(rin SHARED ${RIN_MAIN_SOURCE_FILES})

target_link_libraries(rin fmt::fmt LLVM-12)
