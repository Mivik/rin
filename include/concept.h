
#pragma once

#include "type.h"
#include "utility.h"

namespace rin {

class Concept {
public:
	[[nodiscard]] bool satisfy(Type *type) const {
		// TODO implement this
		return true;
	}

	[[nodiscard]] std::string to_string() const {
		// TODO implement this
		return "any";
	}

	Concept() = default;

	DISABLE_COPY(Concept)
};

} // namespace rin
