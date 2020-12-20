
#pragma once

#include <cassert>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rin {

template<class K, class V>
class LayerMap {
public:
	inline bool empty() const { return keys.empty(); }
	inline int get_depth() const { return keys.size(); }
	inline void add_layer() { keys.emplace_back(); }
	void pop_layer() {
		assert(!empty());
		for (const auto &key : keys.back()) {
			auto iter = base.find(key);
			auto &vec = iter->second;
			vec.pop_back();
			if (vec.empty()) base.erase(iter);
		}
		keys.pop_back();
	}
	inline bool has(const K &key) const { return base.find(key) != base.end(); }
	std::optional<V> try_get(const K &key) const {
		auto iter = base.find(key);
		if (iter == base.end()) return std::nullopt;
		else return iter->second.back();
	}
	// Notice that we throw an error instead of constructing a default
	// value and insert it into current layer when the key is not found
	// in the map.
	V& operator[](const K &key) {
		auto iter = base.find(key);
		assert(iter != base.end());
		return iter->second.back();
	}
	void set(const K &key, const V &value) {
		auto &vec = base[key];
		if (keys.back().insert(key).second)
			vec.push_back(value);
		else vec.back() = value;
	}

	LayerMap() { add_layer(); }
private:
	std::unordered_map<K, std::vector<V>> base;
	std::vector<std::unordered_set<K>> keys;
};

} // namespace rin
