
#pragma once

#include <llvm/IR/Value.h>

#include "type.h"

namespace rin {

class Value {
public:
	Value(Type *type, llvm::Value *llvm):
		type(type), llvm(llvm) {}
private:
	Type * const type;
	llvm::Value * const llvm;
};

} // namespace rin
