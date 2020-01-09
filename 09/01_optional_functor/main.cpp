#include <iostream>
#include <optional>
#include <string>
#include <vector>

template <typename T1, typename F>
auto transform(const std::optional<T1>& opt, F f)
    -> std::optional<decltype(f(opt.value()))>
{
    if (opt) {
        return f(opt.value());
    } else {
        return {};
    }
}


int main(int argc, char* argv[])
{
    std::optional<int> i;

    auto res = transform(i, isalnum);

    return 0;
}
