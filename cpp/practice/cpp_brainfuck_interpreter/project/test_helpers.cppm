module;

#include <cerrno>
#include <cstdio>

export module test_helpers;

import std;

export namespace hlp {
// Runs command, returns console output
inline std::string exec(const std::string & cmd)
{
    std::array<char, 128> buffer;
    std::string output;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(
        popen(cmd.c_str(), "r"), pclose);

    if (!pipe) {
        throw std::runtime_error(std::format("Couldn't run {}", cmd));
    }

    while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe.get())
        != nullptr)
    {
        output += buffer.data();
    }

    return output;
}
} // namespace hlp
