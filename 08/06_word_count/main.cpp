#include <iostream>
#include <vector>
#include <functional>

#include <range/v3/view.hpp>
#include <range/v3/action.hpp>
#include <range/v3/istream_range.hpp>
#include <range/v3/to_container.hpp>

using namespace ranges::v3;
using namespace std::placeholders;

std::string string_to_lower(const std::string &s) {
    return s | view::transform(tolower);
}

std::string string_only_alnum(const std::string &s) {
    return s | view::filter(isalnum);
}

int main(int argc, char *argv[])
{
    const int n = argc <= 1
                    ? 10
                    : atoi(argv[1]);

    const auto words =
               // Kolekcija reci
               istream_range<std::string>(std::cin)

               // Ne zelimo da imamo velika slova
               | view::transform(string_to_lower)

               // Ne zelimo da imamo bilo sta osim slova i cifara
               | view::transform(string_only_alnum)

               // Uklanjamo prazne stringove iz kolekcije
               | view::remove_if(&std::string::empty)

               // Da bismo mogli da sortiramo, potrebna nam je
               // random-access kolekcija
               | to_vector | action::sort;


    const auto results =
               words

               // Grupisemo iste reci
               | view::group_by(std::equal_to<>())

               // Potrebna nam je rec, i koliko puta se ponovila
               | view::transform([] (const auto &group) {
                         const auto begin       = std::begin(group);
                         const auto end         = std::end(group);
                         const int  size        = distance(begin, end);
                         const std::string word = *begin;

                         return std::make_pair(size, word);
                     })

               // Sortiramo rezultat
               | to_vector | action::sort;


    for (auto value: results | view::reverse // Sortirano je u rastucem poretku, obrcemo
                             | view::take(n) // Uzimamo prvih `n` rezultata
            ) {
        std::cout << value.first << " " << value.second << std::endl;
    }

    return 0;
}
