
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

llvm::Value *Value::get_llvm_value() const {
	assert(!is_type_value());
	if (dynamic_cast<Type::Ref *>(type)) {
		if (auto ref = dynamic_cast<Ref::Address *>(ref_value))
			return ref->get_address();
		else throw std::runtime_error("Attempt to use an abstract value");
	}
	return llvm_value;
}

std::optional<Value> Value::cast_to(Codegen &g, Type *to_type) const {
	if (type == to_type) return *this;
	if (auto ref_type = dynamic_cast<Type::Ref *>(type)) {
		if (ref_type->get_sub_type() == to_type) return deref(g);
		if (auto ref_to_type = dynamic_cast<Type::Ref *>(to_type))
			return g.create_ref_value(ref_to_type, llvm_value);
	}
	return std::nullopt;
}

Value Value::pointer_subscript(Codegen &g) const {
	auto ptr_type = dynamic_cast<Type::Pointer *>(type);
	if (!ptr_type)
		g.error("Subscripting a non-pointer type: {}", type->to_string());
	return g.create_ref_value(
		g.get_context().get_ref_type(
			ptr_type->get_sub_type(),
			ptr_type->is_const()
		),
		llvm_value
	);
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
