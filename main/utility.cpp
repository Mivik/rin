
#include "utility.h"

namespace rin {

std::string add_indent(const std::string &str) {
	std::string ret;
	size_t begin = 0;
	while (~begin) {
		ret += '\t';
		const size_t nxt = str.find('\n', begin) + 1;
		if (nxt)
			ret.append(str, begin, nxt);
		else {
			ret.append(str, begin, str.size());
			break;
		}
		begin = nxt;
	}
	return ret;
}

} // namespace rin