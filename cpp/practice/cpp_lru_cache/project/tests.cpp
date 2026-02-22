#include "lru_cache/lru_cache.h"

#include <gtest/gtest.h>

#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

enum class ActionType {
    Get,
    Put,
    Clear,
    Size
};

struct Action {
    ActionType act_type;
    int key = 0;
    std::string val;
    std::variant<std::size_t, std::string> ret_val;    
};

const std::string EX = "ex";

std::string to_string(ActionType const & at, int key, std::string const & val);

std::string get_history(std::string & history);

void call_action(Action const & action, cache::LRUCache & cache, std::string & history);

TEST(LRU, MaxSizeZero)
{
    cache::LRUCache cache(0);
    cache.put(1, "1");
    const std::size_t ms = cache.max_size();
    ASSERT_EQ(ms, 0) << "When initialized with max_size=0 LRUCache::max_size() must return 0 but returns " << ms;

    const std::size_t s = cache.size();
    ASSERT_EQ(s, 0) << "When initialized with max_size=0 LRUCache::size() must always return 0 but returns " << s << " after putting 1 element to cache";

    const auto p = cache.get(1);
    ASSERT_FALSE(p.second) << "Method get() must return pair {\"\", false} if cache max_size=0";
    ASSERT_EQ(p.first, "")<< "Method get() must return pair {\"\", false} if cache max_size=0";
}

TEST(LRU, MissingKey)
{
    cache::LRUCache cache(1);
    const auto p = cache.get(1);
    ASSERT_FALSE(p.second) << "Method get() must return pair {\"\", false} if key doesn't exist";

}

TEST(LRU, KeepElementsWithinCapacity)
{
    const std::size_t max_size = 100;
    const std::size_t elems_count = 200;

    cache::LRUCache cache(max_size);

    for(int k = 1; k <= elems_count; ++k)
    {
        const std::string v = std::to_string(k);
        cache.put(k, v);
        const std::size_t plan = k <= max_size ? k : max_size;

        const std::size_t fact = cache.size();
        ASSERT_EQ(fact, plan) << "Cache was inited with max_size=" << max_size << ". "
        "After " << k << " times of calling put() method size() must return " << plan << " but returned " << fact << "\n";

        ASSERT_EQ(cache.max_size(), max_size) << "After initialization with max_size=" << max_size << " LRUCache::max_size() must always return " << max_size;
    }
}

TEST(LRU, SimplePutGetSizeClearFor1Element)
{
    for(std::size_t max_size = 1; max_size < 4; ++max_size)
    {
        cache::LRUCache cache(max_size);
        const int k = 9;
        const std::string v = "val";

        cache.put(k, v);
        const auto fact = cache.get(k);
        ASSERT_TRUE(fact.second) << "Method get() must return pair {key_value, true} if key exists";
        ASSERT_EQ(v, fact.first) << "LRUCache is inited with max_size=" << max_size << ". "
        "After calling put(" << k << ", \""<< v << "\") method get(" << k << ") must return \"" << v << "\" key but returned \"" << fact.first << "\"";

        const std::size_t fact_size = cache.size();
        ASSERT_EQ(fact_size,1) << "After insertion of 1 element method size() must return 1 but returned " << fact_size;

        cache.clear();

        const std::size_t fact_size_clear = cache.size();
        ASSERT_EQ(fact_size_clear, 0) << "After calling clear() method size() must return 0 but returned " << fact_size_clear;
        ASSERT_EQ(cache.max_size(), max_size) << "After initialization with max_size=" << max_size << " LRUCache::max_size() must always return " << max_size;
    }
}

constexpr std::size_t zero = 0;

TEST(LRU, LRUStrategyCapacity2)
{
    const std::size_t max_size = 2;
    cache::LRUCache cache(max_size);
    std::string history = "\nInited cache with max_size=" + std::to_string(max_size) + "\n";
                                                                                    //    Ключи
    std::vector<Action> actions = {                                                 // <- Старые 
                                                                                    //    Свежие ->                                                                      
        Action{.act_type = ActionType::Put, .key = 1, .val = "1", .ret_val = zero}, // 1
        Action{.act_type = ActionType::Put, .key = 2, .val = "2", .ret_val = zero}, // 1 2
        Action{.act_type = ActionType::Get, .key = 1, .val = "", .ret_val = "1"},   // 2 1
        Action{.act_type = ActionType::Put, .key = 3, .val = "3", .ret_val = zero}, // 1 3
        Action{.act_type = ActionType::Get, .key = 2, .val = "", .ret_val = EX},    // 1 3
        Action{.act_type = ActionType::Put, .key = 4, .val = "4", .ret_val = zero}, // 3 4
        Action{.act_type = ActionType::Get, .key = 1, .val = "", .ret_val = EX},    // 3 4
        Action{.act_type = ActionType::Get, .key = 3, .val = "", .ret_val = "3"},   // 4 3
        Action{.act_type = ActionType::Get, .key = 4, .val = "", .ret_val = "4"},   // 3 4
    };

    for (auto const & action : actions)
        call_action(action, cache, history);
}

TEST(LRU, LRUStrategyWithCallingSize)
{
    const std::size_t max_size = 2;
    cache::LRUCache cache(max_size);
    std::string history = "\nInited cache with max_size=" + std::to_string(max_size) + "\n";
                                                                                    //    Ключи
    std::vector<Action> actions = {                                                 // <- Старые 
                                                                                    //    Свежие ->
        Action{.act_type = ActionType::Size, .key = 0, .val = "", .ret_val = zero},                                                                           
        Action{.act_type = ActionType::Put, .key = 1, .val = "1", .ret_val = zero}, // 1
        Action{.act_type = ActionType::Size, .key = 0, .val = "", .ret_val = std::size_t{1}},
        Action{.act_type = ActionType::Put, .key = 2, .val = "2", .ret_val = zero}, // 1 2
        Action{.act_type = ActionType::Size, .key = 0, .val = "", .ret_val = std::size_t{2}},
        Action{.act_type = ActionType::Put, .key = 3, .val = "3", .ret_val = zero}, // 2 3
        Action{.act_type = ActionType::Get, .key = 1, .val = "", .ret_val = EX},    // 2 3
        Action{.act_type = ActionType::Get, .key = 2, .val = "", .ret_val = "2"},   // 3 2
        Action{.act_type = ActionType::Put, .key = 4, .val = "4", .ret_val = zero}, // 2 4
        Action{.act_type = ActionType::Size, .key = 0, .val = "", .ret_val = std::size_t{2}},
        Action{.act_type = ActionType::Get, .key = 4, .val = "", .ret_val = "4"},   // 2 4
        Action{.act_type = ActionType::Put, .key = 5, .val = "5", .ret_val = zero}, // 4 5
        Action{.act_type = ActionType::Get, .key = 3, .val = "", .ret_val = EX},    // 4 5
        Action{.act_type = ActionType::Get, .key = 4, .val = "", .ret_val = "4"},   // 5 4
        Action{.act_type = ActionType::Get, .key = 5, .val = "", .ret_val = "5"},   // 4 5
        Action{.act_type = ActionType::Size, .key = 0, .val = "", .ret_val = std::size_t{2}},
    };

    for (auto const & action : actions)
        call_action(action, cache, history);
}

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

std::string to_string(ActionType const & at, int key, std::string const & val)
{
    switch(at)
    {
        case ActionType::Get:
            return "get(" + std::to_string(key) + ")";
        case ActionType::Put:
            return "put(" + std::to_string(key) + ", \"" + val + "\")";
        case ActionType::Clear:
            return "clear()";
        case ActionType::Size:
            return "size()";
        default:
            throw std::runtime_error("Unexpected action");
    }
}

std::string get_history(std::string & history)
{
    return "History of actions with cache:\n" + history + "\nError:\n";
}

void call_action(Action const & action, cache::LRUCache & cache, std::string & history)
{
    history += "\nCalling " + to_string(action.act_type, action.key, action.val) + "\n";

    switch(action.act_type)
    {
        case ActionType::Get:
            {
                const auto plan = std::get<std::string>(action.ret_val);
                const auto fact = cache.get(action.key);
                if (plan == EX)
                {
                    ASSERT_FALSE(fact.second) << get_history(history) << "get() must return {\"\", false} for non-existent key";
                }
                else
                {
                    ASSERT_EQ(fact.first, plan) << get_history(history) << "get() returned \"" << fact.first << "\" but expected value is \"" << plan << "\""; 
                }
            }
            break;

        case ActionType::Put:
            cache.put(action.key, action.val);
            break;
        case ActionType::Clear:
            cache.clear();
            break;
        case ActionType::Size:
            {
                const auto plan = std::get<std::size_t>(action.ret_val);
                const std::size_t fact = cache.size();
                ASSERT_EQ(fact, plan) << get_history(history) << "size() returned " << fact << " but expected value is " << plan; 
            }
            break;
        default:
            throw std::runtime_error("Unexpected action");
    }
}
