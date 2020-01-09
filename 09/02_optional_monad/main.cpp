#include <algorithm>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

std::optional<std::string> user_full_name(const std::string& login)
{
    return { "Full name for " + login };
}

std::optional<std::string> to_html(const std::string& text)
{
    return { "<b>" + text + "</b>" };
}


template <typename T, typename F>
auto transform(const std::optional<T>& opt, F f)
    -> decltype(std::make_optional(f(opt.value())))
{
    if (opt) {
        return std::make_optional(f(opt.value()));
    } else {
        return {};
    }
}


template <typename T>
std::optional<T> join(const std::optional<std::optional<T>>& opt)
{
    if (opt) {
        return opt.value();
    } else {
        return {};
    }
}


template <typename T, typename F>
auto mbind(const std::optional<T>& opt, F f)
    -> decltype(f(opt.value()))
{
    if (opt) {
        return f(opt.value());
    } else {
        return {};
    }
}


template <typename F>
struct mbind_helper {
    F f;
};

template <typename F>
mbind_helper<F> mbind(F f)
{
    return {f};
}

template <typename T, typename F>
auto operator|(const std::optional<T> &xs, const mbind_helper<F> &helper)
{
    return mbind(xs, helper.f);
}


template <typename T>
std::ostream& operator<< (std::ostream& out, const std::optional<T> &value)
{
    if (value) {
        out << "value:" << *value;
    } else {
        out << "empty";
    }

    return out;
}


int main(int argc, char* argv[])
{
    std::optional<std::string> login;

    std::cout
        << join(transform(
               join(transform(
                   login,
                   user_full_name)),
               to_html))
        << std::endl;

    login = "jsmith";

    std::cout
        << join(transform(
               join(transform(
                   login,
                   user_full_name)),
               to_html))
        << std::endl;

    login.reset();

    std::cout
        << mbind(
               mbind(
                   login,
                   user_full_name),
               to_html)
        << std::endl;

    login = "jsmith";

    std::cout
        << mbind(
               mbind(
                   login,
                   user_full_name),
               to_html)
        << std::endl;

    login.reset();

    std::cout
        << (login | mbind(user_full_name) | mbind(to_html))
        << std::endl;

    login = "jsmith";

    std::cout
        << (login | mbind(user_full_name) | mbind(to_html))
        << std::endl;

    return 0;
}
