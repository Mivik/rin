
#include <fstream>
#include <iostream>

#include "llvm_stuff.h"

#include "args.hxx"

#include "parser.h"

namespace rin::cli {

enum class OutputFormat : uint8_t {
	BITCODE,
	IR,
	OBJECT,
};

struct Config {
	OutputFormat format = OutputFormat::OBJECT;
	std::string input_file, output_file;
};

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

std::optional<int> parse_args(Config &c, int argc, char *argv[]) {
	std::unordered_map<std::string, OutputFormat> map{
		{ "bitcode", OutputFormat::BITCODE },
		{ "ir",      OutputFormat::IR },
		{ "object",  OutputFormat::OBJECT },
	};

	args::ArgumentParser parser(
		"This is the compiler for rin programming language.",
		"For more information, visit https://github.com/Mivik/rin. Reporting issues is welcome."
	);
	args::HelpFlag help(parser, "help", "Display this help menu", { 'h', "help" });
	args::MapFlag<std::string, OutputFormat> output_format(
		parser,
		"format",
		"Specific the output format",
		{ 'f', "format" },
		map, OutputFormat::OBJECT
	);
	args::Positional<std::string> input_file(parser, "input-file", "The input source file");
	args::Positional<std::string> output_file(parser, "output-file", "The output file");
	try {
		parser.ParseCLI(argc, argv);
	} catch (const args::Help &) {
		std::cout << parser;
		return 0;
	} catch (const args::ParseError &e) {
		std::cerr << e.what() << std::endl;
		std::cerr << parser;
		return -1;
	}
	catch (const args::ValidationError &e) {
		std::cerr << e.what() << std::endl;
		std::cerr << parser;
		return -1;
	}

	c.format = args::get(output_format);

	if (!input_file)
		throw std::runtime_error("Input file must be provided");
	c.input_file = args::get(input_file);

	if (output_file) c.output_file = args::get(output_file);
	else {
		std::string output_suffix;
		switch (c.format) {
			case OutputFormat::BITCODE:
				output_suffix = ".bc";
				break;
			case OutputFormat::IR:
				output_suffix = ".ir";
				break;
			case OutputFormat::OBJECT:
				output_suffix = ".o";
				break;
		}

		auto index = c.input_file.find_last_of('.');
		if (index == std::string::npos) index = c.input_file.size();
		c.output_file = c.input_file.substr(0, index) + output_suffix;
	}

	return std::nullopt;
}

int main(int argc, char *argv[]) {
	Config config;
	if (auto result = parse_args(config, argc, argv))
		return *result;

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

	Context ctx;
	Codegen g(ctx);
	auto code = read_file(config.input_file);
	Parser parser(code);
	auto top_level = parser.take_top_level();
	top_level->codegen(g);
	auto module = g.finalize();

	llvm::legacy::PassManager pass;
	add_default_passes(pass);

	std::error_code error_code;

	llvm::raw_fd_ostream dest(config.output_file, error_code, llvm::sys::fs::OpenFlags::OF_None);
	switch (config.format) {
		case OutputFormat::BITCODE: {
			llvm::WriteBitcodeToFile(*module, dest);
			break;
		}
		case OutputFormat::IR: {
			module->print(dest, nullptr);
			break;
		}
		case OutputFormat::OBJECT: {
			if (target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile))
				throw std::runtime_error("Failed to emit object file");
			pass.run(*module);
			break;
		}
	}
	dest.flush();
	return 0;
}

} // namespace rin::cli

int main(int argc, char *argv[]) {
	return rin::cli::main(argc, argv);
}
