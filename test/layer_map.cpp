
#include <gtest/gtest.h>

#include "layer_map.h"

namespace rin {

TEST(layer_map, basic) {
	LayerMap<int, int> mp;
	mp.add_layer();
	mp.set(0, 1);
	mp.set(2, 3);
	mp.add_layer();
	mp.set(2, 123);
	mp.set(5, 3);
	EXPECT_EQ(mp[5], 3);
	EXPECT_EQ(mp[0], 1);
	EXPECT_EQ(mp[2], 123);
	mp.pop_layer();
	EXPECT_EQ(mp[2], 3);
	EXPECT_FALSE(mp.has(3));
	EXPECT_FALSE(mp.has(5));
	EXPECT_EQ(mp.try_get(5), std::nullopt);
	EXPECT_EQ(mp.try_get(2), 3);
}

TEST(layer_map, handle) {
	LayerMap<int, int> mp;
	mp.add_layer();
	mp.set(0, 5);
	auto handle = mp.handle_of(0);
	mp.add_layer();
	mp.set(0, 12);
	*handle = 11;
	EXPECT_EQ(mp[0], 12);
	mp.pop_layer();
	EXPECT_EQ(mp[0], 11);
}

} // namespace rin
