#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "foo.hpp"

TEST(foo, value)
{
    foo f;
    EXPECT_EQ(1, f.value(1));
}
