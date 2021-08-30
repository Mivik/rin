
#pragma once

#include "value.h"

namespace rin {

class Ref {
public:
	class Address;

	virtual ~Ref() = default;

	virtual Value load(Codegen &g) = 0;
	virtual void store(Codegen &g, Value value) = 0;
	virtual Ref *get_element(Codegen &g, llvm::ArrayRef<unsigned> indices) = 0;

	[[nodiscard]] Type::Ref *get_type() const { return type; }
	[[nodiscard]] bool is_const() const { return type->is_const(); }
protected:
	explicit Ref(Type::Ref *type): type(type) {}

	Type::Ref *type;
};

#define OVERRIDE \
    Value load(Codegen &g) override; \
    void store(Codegen &g, Value value) override; \
	Ref *get_element(Codegen &g, llvm::ArrayRef<unsigned> indices) override;

class Ref::Address final : public Ref {
public:
	[[nodiscard]] llvm::Value *get_address() const { return address; }

	OVERRIDE
private:
	Address(Type::Ref *type, llvm::Value *address):
		Ref(type),
		address(address) {}

	llvm::Value *address;

	friend class Codegen;
};

} // namespace rin
