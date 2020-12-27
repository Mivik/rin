
#pragma once

#include <string>
#include <utility>

#include "type.h"

namespace rin {

class Prototype {
public:
	Prototype(
		std::string name,
		Type::Function *type,
		std::vector<std::string> param_names
	): name(std::move(name)), type(type), param_names(std::move(param_names)) {}

	[[nodiscard]] inline const std::string &get_name() const { return name; }
	[[nodiscard]] inline Type::Function *get_function_type() const { return type; }
	[[nodiscard]] inline const std::vector<std::string> &get_parameter_names() const {
		return param_names;
	}

	[[nodiscard]] std::string to_string() const;
private:
	std::string name;
	Type::Function *type;
	std::vector<std::string> param_names;
};

} // namespace rin
