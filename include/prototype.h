
#pragma once

#include <string>

#include "type.h"

namespace rin {

class Prototype {
public:
	Prototype(
		const std::string &name,
		Type::Function *type,
		const std::vector<std::string> &param_names
	): name(name), type(type), param_names(param_names) {}

	inline const std::string& get_name() const { return name; }
	inline Type::Function* get_function_type() const { return type; }
	inline const std::vector<std::string> &get_parameter_names() const {
		return param_names;
	}

	std::string to_string() const;
private:
	std::string name;
	Type::Function *type;
	std::vector<std::string> param_names;
};

} // namespace rin
