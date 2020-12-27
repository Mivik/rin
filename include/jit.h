
#pragma once

#include <atomic>
#include <functional>
#include <string>
#include <type_traits>

#include <llvm/ExecutionEngine/MCJIT.h>

namespace rin {

class JITEngine {
public:
	explicit JITEngine(std::unique_ptr<llvm::Module> module);

	template<class R, class... Args>
	std::function<R(Args...)> find_function(const std::string &name) {
		return (std::add_pointer_t<R(Args...)>)llvm->getFunctionAddress(name);
	}

	~JITEngine() { delete llvm; }
private:
	static std::atomic_bool initialized;

	llvm::ExecutionEngine *llvm;
	std::string error_msg;
};

} // namespace rin
