#include <vector>
#include <string>
#include <iostream>

using std::string;
using std::vector;

void salted_shuffle(string &input, const string salt) {
    auto i = input.end() - 1;
    unsigned s = 0;
    unsigned sum = salt[s];
    for (; i != input.begin(); // leaves the first char alone
         --i, s = (s + 1) % salt.length(), sum += salt[s]) {
        decltype(i) j =
            input.begin() + (sum + s + salt[s]) % (i - input.begin());
        auto k = *i;
        *i = *j;
        *j = k;
    }
}

int main() {
    string s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
    // string r = salted_shuffle(s, s);
    for (unsigned i = 0; i < 2000000; ++i) {
        salted_shuffle(s, s);
    }
    std::cout << s << std::endl;
    return 0;
}
