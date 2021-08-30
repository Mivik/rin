
#pragma once

#include "layer_map.h"
#include "value.h"

namespace rin {

class Ref {
public:
	class Address;

	virtual ~Ref() = default;

	[[nodiscard]] virtual Value load(Codegen &g) = 0;
	virtual void store(Codegen &g, Value value) = 0;
	[[nodiscard]] virtual Ref *get_element(Codegen &g, const std::vector<unsigned> &indices) = 0;

	[[nodiscard]] Ref *get_element(Codegen &g, unsigned index) {
		return get_element(g, std::vector<unsigned>{ index });
	}

	[[nodiscard]] Type::Ref *get_type() const { return type; }
	[[nodiscard]] bool is_const() const { return type->is_const(); }
protected:
	explicit Ref(Type::Ref *type): type(type) {}

	Type::Ref *type;
};

class Ref::Address final : public Ref {
public:
	[[nodiscard]] llvm::Value *get_address() const { return address; }

	[[nodiscard]] Value load(Codegen &g) override;
	void store(Codegen &g, Value value) override;
	[[nodiscard]] Ref *get_element(Codegen &g, const std::vector<unsigned> &indices) override;
private:
	Address(Type::Ref *type, llvm::Value *address):
		Ref(type),
		address(address) {}

	llvm::Value *address;

	friend class Codegen;
};

} // namespace rin
