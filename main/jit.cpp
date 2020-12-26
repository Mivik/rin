
#include "jit.h"

#include <llvm/Support/TargetSelect.h>

namespace rin {

std::atomic_bool JITEngine::initialized(false);

JITEngine::JITEngine(std::unique_ptr<llvm::Module> module) {
	if (!initialized.exchange(true)) {
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();
		llvm::InitializeNativeTarget();
		LLVMLinkInMCJIT();
	}
	llvm = llvm::EngineBuilder(std::move(module))
		.setErrorStr(&error_msg)
		.setVerifyModules(true)
		.create();
}

} // namespace rin
