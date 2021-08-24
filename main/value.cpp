
#include "codegen.h"
#include "value.h"

namespace rin {

Value Value::deref(Codegen &g) const {
	auto ref_type = dynamic_cast<Type::Ref *>(type);
	if (!ref_type) return *this;
	auto sub = ref_type->get_sub_type();
	return {
		sub,
		g.get_builder()->CreateLoad(sub->get_llvm(), llvm_value)
	};
}

Value Value::pointer_subscript(Codegen &g) const {
	auto ptr_type = dynamic_cast<Type::Pointer *>(type);
	if (!ptr_type)
		g.error("Subscripting a non-pointer type: {}", type->to_string());
	return {
		g.get_context().get_ref_type(
			ptr_type->get_sub_type(),
			ptr_type->is_const()
		),
		llvm_value
	};
}

Value Value::pointer_subscript(Codegen &g, Value index) const {
	auto ptr_type = dynamic_cast<Type::Pointer *>(type);
	if (!ptr_type)
		g.error("Subscripting a non-pointer type: {}", type->to_string());
	// TODO check signed index
	if (!dynamic_cast<Type::Int *>(index.type))
		g.error("Unknown type for pointer subscript: {}", index.type->to_string());
	return {
		g.get_context().get_ref_type(
			ptr_type->get_sub_type(),
			ptr_type->is_const()
		),
		g.get_builder()->CreateGEP(llvm_value, index.llvm_value)
	};
}

} // namespace rin
