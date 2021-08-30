
#include "codegen.h"
#include "ref.h"
#include "value.h"

namespace rin {

Value::Value(Ref *ref): // NOLINT(cppcoreguidelines-pro-type-member-init)
	type(ref->get_type()),
	ref_value(ref) {}

Value Value::deref(Codegen &g) const {
	if (get_kind() != Kind::Ref) return *this;
	return ref_value->load(g);
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
