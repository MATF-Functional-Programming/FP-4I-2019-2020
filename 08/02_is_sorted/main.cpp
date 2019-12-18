#include <iostream>
#include <string>
#include <numeric>
#include <vector>

template <typename T>
bool is_sorted(const T& xs)
{
    return std::inner_product(
        std::cbegin(xs), std::cend(xs) - 1,
        std::cbegin(xs) + 1,
        true,
        [](const bool &x, const bool &y) { return x && y; },
        [](const auto &x, const auto &y) { return x <= y; }
    );
}

int main(int argc, char *argv[])
{
    const std::string text = "Hooloovoo";
    std::cerr << text << ": " << is_sorted(text) << std::endl;

    const std::string word = "Almost";
    std::cerr << word << ": " << is_sorted(word) << std::endl;

    const std::vector<int> numbers{ 1, 2, 3, 3, 4, 5, 6, 6, 7 };
    std::cerr << "numbers: " << is_sorted(word) << std::endl;

    return 0;
}