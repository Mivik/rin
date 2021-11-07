
#include "codegen.h"
#include "ref.h"

namespace rin {

inline llvm::ArrayRef<llvm::Constant *> as_constant(const std::vector<llvm::Value *> &indices) {
	return { reinterpret_cast<llvm::Constant *const *>(indices.data()), indices.size() };
}

inline unsigned as_int(llvm::Value *value) {
	auto const_int = llvm::dyn_cast<llvm::ConstantInt>(value);
	assert(const_int);
	return const_int->getZExtValue();
}

Value Ref::Address::load(Codegen &g) {
	return g.create_value(
		type->get_sub_type(),
		g.get_builder()->CreateLoad(address)
	);
}

void Ref::Address::store(Codegen &g, Value value) {
	assert(value.get_type() == type->get_sub_type());
	g.get_builder()->CreateStore(value.get_llvm_value(), address);
}

Ref *Ref::Address::get_element(Codegen &g, const std::vector<llvm::Value *> &indices) {
	assert(!indices.empty());
	auto result_type = type->get_sub_type();
	for (size_t i = 1; i < indices.size(); ++i) {
		unsigned index = -1;
		if (auto const_int = llvm::dyn_cast<llvm::ConstantInt>(indices[i]))
			index = const_int->getZExtValue();
		result_type = result_type->get_element(index);
	}
	// TODO In bound?
	return g.create_ref<Ref::Address>(
		g.get_context().get_ref_type(result_type, type->is_mutable()),
		g.get_builder()->CreateGEP(address, indices)
	);
}

Ref *Ref::Address::cast(Codegen &g, bool mutable_flag) {
	if (mutable_flag == type->is_mutable()) return this;
	return g.create_ref<Ref::Address>(
		g.get_context().get_ref_type(type->get_sub_type(), mutable_flag),
		address
	);
}

Value Ref::Memory::load(Codegen &) { return *ptr; }

void Ref::Memory::store(Codegen &g, Value value) {
	assert(value.get_type() == type->get_sub_type());
	if (!value.is_constant()) g.error("Const variables can only be assigned with constants");
	*ptr = value;
}

Ref *Ref::Memory::get_element(Codegen &g, const std::vector<llvm::Value *> &indices) {
	assert(!indices.empty());
	if (indices.size() == 1) return this;
	Sub *current = g.create_ref<Sub>(g, this, as_int(indices[1]));
	for (size_t i = 2; i < indices.size(); ++i) {
		auto this_index = as_int(indices[i]);
		current = g.create_ref<Sub>(g, current, this_index);
	}
	return current;
}

Ref *Ref::Memory::cast(Codegen &g, bool mutable_flag) {
	if (mutable_flag == type->is_mutable()) return this;
	return g.create_ref<Ref::Memory>(
		g.get_context().get_ref_type(type->get_sub_type(), mutable_flag),
		ptr
	);
}

inline Type::Ref *get_sub_type_as_ref(Codegen &g, Type::Ref *father, unsigned index) {
	return g.get_context().get_ref_type(
		father->get_sub_type()->get_element(index),
		father->is_mutable()
	);
}

Ref::Memory::Sub::Sub(Codegen &g, Memory *root, unsigned int index) // NOLINT(cppcoreguidelines-pro-type-member-init)
	: Ref(get_sub_type_as_ref(g, root->get_type(), index)), is_first(true), index(index), root(root) {}

Ref::Memory::Sub::Sub(Codegen &g, Sub *father, unsigned int index) // NOLINT(cppcoreguidelines-pro-type-member-init)
	: Ref(get_sub_type_as_ref(g, father->get_type(), index)), is_first(false), index(index), father(father) {}

Ref::Memory::Sub::Sub(Type::Ref *type, bool is_first, unsigned index,
					  void *any) // NOLINT(cppcoreguidelines-pro-type-member-init)
	: Ref(type), is_first(is_first), index(index), any(any) {}

std::pair<Ref::Memory *, std::vector<unsigned>> Ref::Memory::Sub::collect(Codegen &g) const {
	std::vector<unsigned> indices;
	const Sub *current = this;
	Memory *root;
	while (true) {
		indices.push_back(current->index);
		if (current->is_first) {
			root = current->root;
			break;
		} else current = current->father;
	}
	std::reverse(indices.begin(), indices.end());
	return { root, indices };
}

Value Ref::Memory::Sub::load(Codegen &g) {
	auto[root, indices] = collect(g);
	return g.create_value(
		type->get_sub_type(),
		llvm::ConstantExpr::getExtractValue(root->get_as_constant(), indices)
	);
}

void Ref::Memory::Sub::store(Codegen &g, Value value) {
	assert(value.get_type() == type->get_sub_type());
	assert(value.is_constant());
	auto[root, indices] = collect(g);
	root->store(g, g.create_value(
		root->type->get_sub_type(),
		llvm::ConstantExpr::getInsertValue(
			root->get_as_constant(),
			llvm::dyn_cast<llvm::Constant>(value.get_llvm_value()),
			indices
		)
	));
}

Ref *Ref::Memory::Sub::get_element(Codegen &g, const std::vector<llvm::Value *> &indices) {
	assert(!indices.empty());
	assert(as_int(indices[0]) == 0);
	Sub *current = this;
	for (size_t i = 1; i < indices.size(); ++i) {
		auto this_index = as_int(indices[i]);
		current = g.create_ref<Sub>(g, current, this_index);
	}
	return current;
}

Ref *Ref::Memory::Sub::cast(Codegen &g, bool mutable_flag) {
	if (mutable_flag == type->is_mutable()) return this;
	return g.create_ref<Ref::Memory::Sub>(
		g.get_context().get_ref_type(type->get_sub_type(), mutable_flag),
		is_first, index, any
	);
}

} // namespace rin
