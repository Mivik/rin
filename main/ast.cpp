
#include <llvm/ADT/APInt.h>

#include "ast.h"
#include "utility.h"

namespace rin {

Ptr<Value> ConstantNode::codegen(Context &ctx) {
	auto type = type_node->codegen(ctx);
	if (auto int_type = dynamic_cast<Type::Int*>(type)) {
		auto llvm_int_type = ptr_cast<llvm::IntegerType>(int_type->get_llvm());
		if (int_type == ctx.get_core().get_boolean_type()) {
			uint64_t value = 0;
			if (str == "true") value = 1;
			else if (str == "false") value = 0;
			else rin_unreachable(("Illegal boolean literal: " + str).data());
			return std::make_unique<Value>(type,
				ptr_cast<llvm::Value>(llvm::ConstantInt::get(llvm_int_type, 1ULL)));
		}
		return std::make_unique<Value>(type,
			ptr_cast<llvm::Value>(llvm::ConstantInt::get(llvm_int_type, str, 10)));
	}
	abort();
}

} // namespace rin
