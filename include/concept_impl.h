
#pragma once

#include "function.h"

namespace rin {

class Concept::Implementation {
public:
	Implementation() = default;
	explicit Implementation(std::vector<Function::Static *> functions): functions(std::move(functions)) {}

	[[nodiscard]] const std::vector<Function::Static *> &get_functions() const { return functions; }

private:
	std::vector<Function::Static *> functions;
};

} // namespace rin
