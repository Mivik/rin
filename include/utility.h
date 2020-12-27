
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

namespace rin {

template<class R, class T>
inline R *direct_cast(T *ptr) { return reinterpret_cast<R *>(ptr); }

template<class R, class T>
inline const R *direct_cast(const T *ptr) { return reinterpret_cast<const R *>(ptr); }

template<class T, class D = std::default_delete<T>>
using Ptr = std::unique_ptr<T, D>;

template<class R, class T, class D>
Ptr<R, D> ptr_cast(Ptr<T, D> &&p) {
	if (auto cast = dynamic_cast<R *>(p.get())) {
		Ptr<R, D> ret(cast, std::move(p.get_deleter()));
		p.release();
		return ret;
	}
	return nullptr;
}

constexpr size_t hash_combine(size_t lhs, size_t rhs) {
	return lhs ^ (rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2));
}

template<
	class A, class B,
	class AH = std::hash<A>, class BH = std::hash<B>
>
struct PairHash {
	inline size_t operator()(const std::pair<A, B> &value) const {
		return hash_combine(AH()(value.first), BH()(value.second));
	}
};

std::string add_indent(const std::string &str);

} // namespace rin
