
#include <iostream>

#include "parser.h"

int main() {
	rin::CoreContext core;
	rin::Context ctx(core);
	std::string str;
	while (true) {
		llvm::outs() << "> ";
		if (!std::getline(std::cin, str)) break;
		rin::Parser parser(str.data());
		auto value = parser.take_expr()->codegen(ctx);
		llvm::outs() << '[' << value.get_type()->to_string() << "] ";
		value.get_llvm()->print(llvm::outs());
		llvm::outs() << '\n';
	}
}