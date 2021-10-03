
#pragma once

#include <exception>
#include <optional>
#include <utility>

#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>

#include "concept.h"
#include "context.h"
#include "layer_map.h"
#include "type.h"

namespace rin {

class Codegen;

class Ref;

class Value {
public:
	enum class Kind {
		Concept,
		Normal,
		Ref,
		Type,
	};

	static inline Value undef(Type *type) {
		return {
			type,
			llvm::UndefValue::get(type->get_llvm())
		};
	}

	Value(): type(nullptr), llvm_value(nullptr) {} // NOLINT(cppcoreguidelines-pro-type-member-init)

	Value(Type *type, llvm::Value *llvm): // NOLINT(cppcoreguidelines-pro-type-member-init)
		type(type), llvm_value(llvm) {
		if (dynamic_cast<Type::Ref *>(type))
			throw std::runtime_error("Ref value cannot be created directly");
	}

	explicit Value(Concept *concept_value): // NOLINT(cppcoreguidelines-pro-type-member-init)
		type(Type::Concept::get_instance()), concept_value(concept_value) {}

	// Type is also a value
	explicit Value(Type *type_value): // NOLINT(cppcoreguidelines-pro-type-member-init)
		type(Type::Self::get_instance()), type_value(type_value) {}

	explicit Value(Ref *ref);

	// TODO is it?
	[[nodiscard]] bool is_constant() const;

	[[nodiscard]] Value deref(Codegen &g) const;

	[[nodiscard]] Kind get_kind() const {
		if (is_type_value()) return Kind::Type;
		if (is_concept_value()) return Kind::Concept;
		if (is_ref_value()) return Kind::Ref;
		return Kind::Normal;
	}

	[[nodiscard]] bool is_normal_value() const { return get_kind() == Kind::Normal; }
	[[nodiscard]] bool is_concept_value() const { return type == Type::Concept::get_instance(); }
	[[nodiscard]] bool is_type_value() const { return type == Type::Self::get_instance(); }
	[[nodiscard]] bool is_ref_value() const { return dynamic_cast<Type::Ref *>(type); }

	[[nodiscard]] bool can_cast_to(Type *to_type) const {
		if (type == to_type) return true;
		if (auto self = dynamic_cast<Type::Ref *>(type)) {
			if (self->get_sub_type() == to_type) return true;
			if (auto other = dynamic_cast<Type::Ref *>(to_type))
				if (self->get_sub_type() == other->get_sub_type()) return true;
		}
		return false;
	}
	[[nodiscard]] std::optional<Value> cast_to(Codegen &g, Type *to_type) const;

	[[nodiscard]] Type *get_type() const { return type; }
	[[nodiscard]] llvm::Value *get_llvm_value() const;
	[[nodiscard]] Concept *get_concept_value() const {
		assert(is_concept_value());
		return concept_value;
	}
	[[nodiscard]] Type *get_type_value() const {
		assert(is_type_value());
		return type_value;
	}
	[[nodiscard]] Ref *get_ref_value() const {
		assert(is_ref_value());
		return ref_value;
	}

	[[nodiscard]] Value pointer_subscript(Codegen &g) const;
	[[nodiscard]] Value pointer_subscript(Codegen &g, Value index) const;

	// TODO remove this in release build
	void dump(llvm::raw_ostream &out = llvm::errs()) const {
		switch (get_kind()) {
			case Kind::Normal: {
				out << '[' << type->to_string() << "] ";
				llvm_value->print(out);
				break;
			}
			case Kind::Ref: {
				out << "[ref] " << type->to_string();
				break;
			}
			case Kind::Type: {
				out << "[type] " << type_value->to_string();
				break;
			}
			case Kind::Concept: {
				out << "[concept] " << concept_value->to_string();
				break;
			}
		}
		out << '\n';
	}
private:
	Type *type;
	union {
		llvm::Value *llvm_value;
		Concept *concept_value;
		Type *type_value;
		Ref *ref_value;
	};
};

} // namespace rin
