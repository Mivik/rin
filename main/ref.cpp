
#include "codegen.h"
#include "ref.h"

namespace rin {

Value Ref::Address::load(Codegen &g) {
	return {
		type->get_sub_type(),
		g.get_builder()->CreateLoad(address)
	};
}

void Ref::Address::store(Codegen &g, Value value) {
	assert(value.get_type() == type->get_sub_type());
	g.get_builder()->CreateStore(value.get_llvm_value(), address);
}

Ref *Ref::Address::get_element(Codegen &g, const std::vector<unsigned> &indices) {
	llvm::OwningArrayRef<llvm::Value *> values(indices.size() + 1);
	auto int32_type = llvm::Type::getInt32Ty(g.get_llvm_context());
	auto result_type = type->get_sub_type();
	values[0] = llvm::ConstantInt::get(int32_type, 0);
	for (size_t i = 0; i < indices.size(); ++i) {
		values[i + 1] = llvm::ConstantInt::get(int32_type, indices[i]);
		result_type = result_type->get_element(indices[i]);
	}
	// TODO In bound?
	return g.create_ref<Ref::Address>(
		g.get_context().get_ref_type(result_type, type->is_const()),
		g.get_builder()->CreateGEP(address, values)
	);
}

} // namespace rin
