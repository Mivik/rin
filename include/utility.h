
#pragma once

#include <cstdlib>

#define DISABLE_COPY(class_name) \
    class_name(const class_name&) = delete; \
    class_name& operator=(const class_name&) = delete;

#define rin_unreachable __builtin_unreachable

namespace rin {

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

} // namespace rin
