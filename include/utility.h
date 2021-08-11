
#pragma once

#include <cstdlib>

#define DISABLE_COPY(class_name) \
    class_name(const class_name&) = delete; \
    class_name& operator=(const class_name&) = delete;

#define RIN_UNREACHABLE __builtin_unreachable

namespace rin {

template<class R, class T>
inline R *direct_cast(T *ptr) { return reinterpret_cast<R *>(ptr); }

template<class R, class T>
inline const R *direct_cast(const T *ptr) { return reinterpret_cast<const R *>(ptr); }

template<class T, class D = std::default_delete<T>>
using Ptr = std::unique_ptr<T, D>;

constexpr size_t hash_combine(size_t lhs, size_t rhs) {
	return lhs ^ (rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2));
}

template<
	class A, class B,
	class AH = std::hash<A>, class BH = std::hash<B>
>
struct PairHash {
	size_t operator()(const std::pair<A, B> &value) const {
		return hash_combine(AH()(value.first), BH()(value.second));
	}
};

template<class V, class VH = std::hash<V>>
struct ArrayHash {
	inline size_t operator()(const std::vector<V> &arr) const {
		size_t res = 0;
		for (auto element : arr) res = hash_combine(res, VH()(element));
		return res;
	}
};

} // namespace rin
