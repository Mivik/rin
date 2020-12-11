
#pragma once

#include <llvm/Support/raw_ostream.h>

#include <cstdlib>
#include <string>

#define DISABLE_COPY(class_name) \
	class_name(const class_name&) = delete; \
	class_name& operator=(const class_name&) = delete;

namespace rin::detail {

[[noreturn]] inline void unreachable_internal(const std::string &msg, uint32_t line) {
	llvm::errs() << "Unreachable code reached\n";
	llvm::errs() << msg << '\n';
	abort();
}

} // namespace rin::detail

#define rin_unreachable(msg) rin::detail::unreachable_internal(msg, __LINE__)

template<class R, class T>
inline R* ptr_cast(T *ptr) { return reinterpret_cast<R*>(ptr); }

template<class R, class T>
inline const R* ptr_cast(const T *ptr) { return reinterpret_cast<const R*>(ptr); }
