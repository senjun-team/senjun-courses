#include "lru_cache.h"

#include <stdexcept>

namespace cache
{
LRUCache::LRUCache(std::size_t max_size)
{
    m_max_size = max_size;
}

std::pair<std::string, bool> LRUCache::get(int key)
{
    auto it_map = m_map.find(key);
    if (it_map == m_map.end())
        return {"", false};

    auto it_list = it_map->second;
    m_list.splice(m_list.begin(), m_list, it_list);
    return {it_list->second, true};
}
    
void LRUCache::put(int key, std::string value)
{   
    if (auto it_map = m_map.find(key); it_map != m_map.end())
    {
        auto it_list = it_map->second;        
        m_list.erase(it_list);
        m_map.erase(it_map);
    }

    m_list.push_front({key, value});
    m_map[key] = m_list.begin();
    
    if (m_map.size() > m_max_size)
    {
        auto it_last = --m_list.end();
        m_map.erase(it_last->first);
        m_list.pop_back();
    }
}

void LRUCache::clear()
{
    m_map.clear();
    m_list.clear();
}

std::size_t LRUCache::size()
{
    return m_map.size();
}

std::size_t LRUCache::max_size()
{
    return m_max_size;
}
} // namespace cache
