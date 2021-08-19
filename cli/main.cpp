
#include <fstream>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include "parser.h"

namespace rin::cli {

static const llvm::Target *target;
static llvm::TargetMachine *target_machine;
static std::string target_triple;

void initialize() {
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();
}

void add_default_passes(llvm::legacy::PassManager &m) {
	m.add(llvm::createConstantHoistingPass());
	m.add(llvm::createInstructionCombiningPass());
	m.add(llvm::createReassociatePass());
	m.add(llvm::createGVNPass());
	m.add(llvm::createCFGSimplificationPass());
	m.add(llvm::createFunctionInliningPass());
}

std::string read_file(const std::string &path) {
	std::string ret;
	std::ifstream in(path);
	if (!in)
		throw std::runtime_error(std::string("Failed to open file: ") + strerror(errno));
	in.seekg(0, std::ifstream::end);
	ret.resize(in.tellg());
	in.seekg(0, std::ifstream::beg);
	if (!in.read(ret.data(), static_cast<std::streamsize>(ret.size())))
		throw std::runtime_error(std::string("Failed to read from file: ") + strerror(errno));
	return ret;
}

int main(int argc, char *argv[]) {
	if (argc < 2)
		throw std::runtime_error("Too less arguments!");

	initialize();
	std::string error_msg;

	target_triple = llvm::sys::getDefaultTargetTriple();
	target = llvm::TargetRegistry::lookupTarget(target_triple, error_msg);

	if (!target)
		throw std::runtime_error(error_msg);
	llvm::StringMap<bool, llvm::MallocAllocator> cpu_features_map;
	llvm::sys::getHostCPUFeatures(cpu_features_map);
	std::string cpu_features;
	// TODO how does this work?
//	for (auto &entry : cpu_features_map)
//		if (entry.getValue()) {
//			cpu_features += entry.getKey();
//			cpu_features += ' ';
//		}
	if (!cpu_features.empty()) cpu_features.pop_back();
	// TODO optimization level
	target_machine = target->createTargetMachine(
		target_triple,
		llvm::sys::getHostCPUName(),
		cpu_features,
		llvm::TargetOptions(),
		llvm::Optional<llvm::Reloc::Model>()
	);

	Context ctx;
	Codegen g(ctx);
	Parser parser(read_file(argv[1]));
	auto top_level = parser.take_top_level();
	top_level->codegen(g);
	auto module = g.finalize();

	std::error_code error_code;
	llvm::raw_fd_ostream dest("output.o", error_code, llvm::sys::fs::OpenFlags::OF_None);

	llvm::legacy::PassManager pass;
	add_default_passes(pass);
	if (target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile))
		throw std::runtime_error("Failed to emit object file");

	pass.run(*module);
	dest.flush();
	return 0;
}

} // namespace rin::cli

int main(int argc, char *argv[]) {
	return rin::cli::main(argc, argv);
}
