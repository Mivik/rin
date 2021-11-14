
#pragma once

#include "type.h"
#include "utility.h"

namespace rin {

class Concept {
public:
	struct FunctionItem {
		Type::Function *type;
		std::string name;
	};

	class Implementation;

	DISABLE_COPY(Concept)

	[[nodiscard]] std::string to_string() const {
		// TODO complete this
		return "concept@" + std::to_string((uintptr_t) this);
	}

	[[nodiscard]] const std::vector<FunctionItem> &get_function_items() const { return function_items; }

private:
	explicit Concept(std::vector<FunctionItem> function_items): function_items(std::move(function_items)) {}

	std::vector<FunctionItem> function_items;

	friend class Context;
};

} // namespace rin
