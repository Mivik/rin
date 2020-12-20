
#include <iostream>

#include "codegen.h"
#include "parser.h"

int main() {
	rin::CoreContext core;
	rin::Context ctx(core);
	std::string str;
	while (true) {
		llvm::outs() << "> ";
		if (!std::getline(std::cin, str)) break;
		str += ';';
		try {
			rin::Parser parser(str.data());
			auto value = parser.take_stmt()->codegen(ctx);
			if (value.get_type() == core.get_void_type()) continue;
			llvm::outs() << '[' << value.get_type()->to_string() << "] ";
			value.get_llvm()->print(llvm::outs());
			llvm::outs() << '\n';
		} catch (const rin::LexException &e) {
			llvm::errs() << "Syntax error: " << e.what() << '\n';
		} catch (const rin::ParseException &e) {
			llvm::errs() << "Syntax error: " << e.what() << '\n';
		} catch (const rin::CodegenException &e) {
			llvm::errs() << "Codegen error: " << e.what() << '\n';
		} catch (const rin::CastException &e) {
			llvm::errs() << "Cast error: " << e.what() << '\n';
		}
	}
}