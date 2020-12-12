
#pragma once

#include <exception>
#include <string>

namespace rin {

class CodegenException : public std::exception {
public:
	const char *what() const noexcept { return msg.data(); }

	CodegenException(const std::string &msg): msg(msg) {}
private:
	std::string msg;
};

} // namespace rin
