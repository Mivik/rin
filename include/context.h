
#pragma once

#include <llvm/IR/IRBuilder.h>

#include "core_context.h"
#include "utility.h"

namespace rin {

class Context {
public:
	inline CoreContext& get_core() { return core; }
	inline llvm::LLVMContext& get_llvm() { return core.get_llvm(); }

	DISABLE_COPY(Context)
private:
	llvm::IRBuilder<> builder;
	CoreContext core;
};

} // namespace rin
