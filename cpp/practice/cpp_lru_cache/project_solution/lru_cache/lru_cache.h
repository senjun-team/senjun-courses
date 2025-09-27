#include <unordered_map>
#include <list>
#include <string>
#include <utility>

namespace cache
{
class LRUCache
{
public:
    LRUCache(std::size_t max_size);
    std::pair<std::string, bool> get(int key);
    void put(int key, std::string value);
    void clear();
    std::size_t size();
    std::size_t max_size();

private:
    std::size_t m_max_size;
    std::list<std::pair<int, std::string>> m_list;
    std::unordered_map<int, std::list<std::pair<int, std::string>>::iterator> m_map;
};
} // namespace cache
