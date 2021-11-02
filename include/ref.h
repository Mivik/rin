
#pragma once

#include "codegen.h"
#include "layer_map.h"
#include "value.h"

namespace rin {

class Ref {
public:
	class Address;

	class Memory;

	virtual ~Ref() = default;

	[[nodiscard]] virtual Value load(Codegen &g) = 0;
	virtual void store(Codegen &g, Value value) = 0;
	[[nodiscard]] virtual Ref *get_element(Codegen &g, const std::vector<llvm::Value *> &indices) = 0;
	[[nodiscard]] virtual Ref *cast(Codegen &g, bool mutable_flag) = 0;

	[[nodiscard]] Ref *get_element(Codegen &g, unsigned index) {
		return get_element(g, std::vector<llvm::Value *>{ g.get_constant_int(index) });
	}

	[[nodiscard]] Ref *get_element(Codegen &g, unsigned index0, unsigned index1) {
		return get_element(g, std::vector<llvm::Value *>{
			g.get_constant_int(index0),
			g.get_constant_int(index1)
		});
	}

	[[nodiscard]] Ref *get_element(Codegen &g, Value index) {
		return get_element(g, std::vector<llvm::Value *>{ index.get_llvm_value() });
	}

	[[nodiscard]] Ref *get_element(Codegen &g, Value index0, Value index1) {
		return get_element(g, std::vector<llvm::Value *>{
			index0.get_llvm_value(),
			index1.get_llvm_value()
		});
	}

	[[nodiscard]] Ref *get_element(Codegen &g, const std::vector<Value> &indices) {
		std::vector<llvm::Value *> llvm_indices(indices.size());
		for (size_t i = 0; i < indices.size(); ++i) llvm_indices[i] = indices[i].get_llvm_value();
		return get_element(g, llvm_indices);
	}

	[[nodiscard]] Type::Ref *get_type() const { return type; }
	[[nodiscard]] bool is_mutable() const { return type->is_mutable(); }
protected:
	explicit Ref(Type::Ref *type): type(type) {}

	Type::Ref *type;
};

#define OVERRIDE \
	[[nodiscard]] Value load(Codegen &g) override; \
	void store(Codegen &g, Value value) override; \
	[[nodiscard]] Ref *get_element(Codegen &g, const std::vector<llvm::Value *> &indices) override; \
	[[nodiscard]] Ref *cast(Codegen &g, bool mutable_flag) override;

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

class Ref::Memory final : public Ref {
public:
	class Sub final : public Ref {
	public:
		OVERRIDE
	private:
		Sub(Codegen &g, Memory *root, unsigned index);
		Sub(Codegen &g, Sub *father, unsigned index);
		Sub(Type::Ref *type, bool is_first, unsigned index, void *any);

		[[nodiscard]] std::pair<Memory *, std::vector<unsigned>> collect(Codegen &g) const;

		bool is_first;
		unsigned index;
		union {
			Memory *root;
			Sub *father;
			void *any;
		};

		friend class Codegen;
	};

	[[nodiscard]] Value *get_address() const { return ptr.get(); }
	[[nodiscard]] llvm::Constant *get_as_constant() const {
		return llvm::dyn_cast<llvm::Constant>(ptr->get_llvm_value());
	}

	OVERRIDE
private:
	explicit Memory(Type::Ref *type): Memory(type, std::make_shared<Value>()) {}
	Memory(Type::Ref *type, SPtr <Value> ptr):
		Ref(type), ptr(std::move(ptr)) {}

	SPtr <Value> ptr;

	friend class Codegen;
};

} // namespace rin
