
#pragma once

#include <exception>
#include <string>
#include <utility>

namespace rin {

class CodegenException : public std::exception {
public:
	[[nodiscard]] const char *what() const noexcept override { return msg.data(); }

	explicit CodegenException(std::string msg): msg(std::move(msg)) {}
private:
	std::string msg;
};

} // namespace rin
