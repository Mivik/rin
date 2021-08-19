
cmake_minimum_required(VERSION 3.16)

project(rin)

set(CMAKE_CXX_STANDARD 17)

enable_testing()

find_package(LLVM REQUIRED CONFIG)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

llvm_map_components_to_libnames(llvm_libs
	Support
	Core
	MCJIT

	AArch64AsmParser
	AMDGPUAsmParser
	ARMAsmParser
	AsmParser
	AVRAsmParser
	BPFAsmParser
	HexagonAsmParser
	LanaiAsmParser
	MCParser
	MipsAsmParser
	MIRParser
	MSP430AsmParser
	PowerPCAsmParser
	RISCVAsmParser
	SparcAsmParser
	SystemZAsmParser
	WebAssemblyAsmParser
	X86AsmParser

	AArch64CodeGen
	AMDGPUCodeGen
	ARMCodeGen
	AVRCodeGen
	BPFCodeGen
	CodeGen
	HexagonCodeGen
	LanaiCodeGen
	MipsCodeGen
	MSP430CodeGen
	NVPTXCodeGen
	PowerPCCodeGen
	RISCVCodeGen
	SparcCodeGen
	SystemZCodeGen
	WebAssemblyCodeGen
	X86CodeGen
	XCoreCodeGen
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(main)
add_subdirectory(test)
add_subdirectory(cli)
