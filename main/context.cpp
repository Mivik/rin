
#include "context.h"
#include "value.h"

namespace rin {

std::optional<Value> Context::lookup_value(const std::string &name) {
	return value_map.try_get(name);
}

} // namespace rin
