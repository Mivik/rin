
#pragma once

#include <llvm/IR/IRBuilder.h>

#include "core_context.h"
#include "layer_map.h"
#include "utility.h"

namespace rin {

class Value;

class Context {
public:
	explicit Context(CoreContext &core):
		core(core), builder(core.get_llvm()) {}
	inline CoreContext& get_core() { return core; }
	inline llvm::LLVMContext& get_llvm() { return core.get_llvm(); }
	inline llvm::IRBuilder<>& get_builder() { return builder; }

	DISABLE_COPY(Context)
private:
	CoreContext &core;
	llvm::IRBuilder<> builder;
};

} // namespace rin
