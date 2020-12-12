
#pragma once

#include <llvm/Support/raw_ostream.h>

#include <cstdlib>
#include <memory>
#include <string>

#define DISABLE_COPY(class_name) \
	class_name(const class_name&) = delete; \
	class_name& operator=(const class_name&) = delete;

namespace rin::detail {

[[noreturn]] inline void unreachable_internal(const std::string &msg, const char *file, uint32_t line) {
	llvm::errs() << "Unreachable code reached at " << file << ':' << line << '\n';
	llvm::errs() << msg << '\n';
	abort();
}

} // namespace rin::detail

#define rin_unreachable(msg) rin::detail::unreachable_internal(msg, __FILE__, __LINE__)

template<class R, class T>
inline R* ptr_cast(T *ptr) { return reinterpret_cast<R*>(ptr); }

template<class R, class T>
inline const R* ptr_cast(const T *ptr) { return reinterpret_cast<const R*>(ptr); }

template<class T>
using Ptr = std::unique_ptr<T>;
