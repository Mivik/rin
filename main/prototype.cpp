
#include "prototype.h"

namespace rin {

std::string Prototype::to_string() const {
	std::string ret = "fn ";
	if (auto receiver_type = type->get_receiver_type()) {
		ret += receiver_type->to_string();
		ret += '.';
	}
	ret += name;
	ret += '(';
	const auto &param_types = type->get_parameter_types();
	for (size_t i = 0; i < param_names.size(); ++i) {
		ret += param_types[i]->to_string();
		ret += ' ';
		ret += param_names[i];
		if (i != param_names.size() - 1) ret += ", ";
	}
	ret += ')';
	if (auto result_type = type->get_result_type()) {
		ret += ": ";
		ret += result_type->to_string();
	}
	return ret;
}

} // namespace rin
