
#include <fstream>
#include <iostream>

#include <boost/program_options.hpp>

#include <llvm/Bitcode/BitcodeWriter.h>
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

namespace po = boost::program_options;

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

std::optional<po::variables_map> parse_args(int argc, char *argv[]) {
	po::options_description desc("Available options");
	desc.add_options()
		("help", "produce help message")
		(
			"format,f",
			po::value<std::string>()->default_value("object"),
			"specify the output format (object, ir, bitcode)"
		)
		("input-file", po::value<std::string>(), "set input file")
		("output-file,o", po::value<std::string>(), "set output file");

	po::positional_options_description pos;
	pos.add("input-file", 1);

	po::variables_map vm;
	po::store(
		po::command_line_parser(argc, argv)
			.options(desc)
			.positional(pos).run(),
		vm
	);
	po::notify(vm);

	if (vm.count("help")) {
		std::cout << desc << '\n';
		return std::nullopt;
	}

	if (!vm.count("input-file"))
		throw std::runtime_error("Input file must be provided");

	return vm;
}

int main(int argc, char *argv[]) {
	auto opt = parse_args(argc, argv);
	if (!opt) return 0;
	auto vm = *opt;

	initialize();
	std::string error_msg;

	target_triple = llvm::sys::getDefaultTargetTriple();
	target = llvm::TargetRegistry::lookupTarget(target_triple, error_msg);

	if (!target)
		throw std::runtime_error(error_msg);
	llvm::StringMap<bool, llvm::MallocAllocator> cpu_features_map;
	llvm::sys::getHostCPUFeatures(cpu_features_map);
	std::string cpu_features;
	for (auto &entry : cpu_features_map)
		if (entry.getValue()) {
			cpu_features += '+';
			cpu_features += entry.getKey();
			cpu_features += ',';
		}
	if (!cpu_features.empty()) cpu_features.pop_back();
	// TODO optimization level
	target_machine = target->createTargetMachine(
		target_triple,
		llvm::sys::getHostCPUName(),
		cpu_features,
		llvm::TargetOptions(),
		llvm::Optional<llvm::Reloc::Model>()
	);

	auto input_file = vm["input-file"].as<std::string>();

	Context ctx;
	Codegen g(ctx);
	auto code = read_file(input_file);
	Parser parser(code);
	auto top_level = parser.take_top_level();
	top_level->codegen(g);
	auto module = g.finalize();

	llvm::legacy::PassManager pass;
	add_default_passes(pass);

	auto format = vm["format"].as<std::string>();

	std::string output_suffix;
	if (format == "object") output_suffix = ".o";
	else if (format == "bitcode") output_suffix = ".bc";
	else if (format == "ir") output_suffix = ".ir";
	else throw std::runtime_error("Unrecognized output format: " + format);

	std::string output_file;
	{
		auto iter = vm.find("output-file");
		if (iter == vm.end()) {
			auto index = input_file.find_last_of('.');
			if (index == std::string::npos) index = input_file.size();
			output_file = input_file.substr(0, index) + output_suffix;
		} else output_file = iter->second.as<std::string>();
	}

	std::error_code error_code;

	llvm::raw_fd_ostream dest(output_file, error_code, llvm::sys::fs::OpenFlags::OF_None);
	if (format == "object") {
		if (target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile))
			throw std::runtime_error("Failed to emit object file");
		pass.run(*module);
	} else if (format == "bitcode") {
		llvm::WriteBitcodeToFile(*module, dest);
	} else if (format == "ir") {
		module->print(dest, nullptr);
	}

	dest.flush();

	return 0;
}

} // namespace rin::cli

int main(int argc, char *argv[]) {
	return rin::cli::main(argc, argv);
}
