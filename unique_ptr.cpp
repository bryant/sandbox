#include <memory>
#include <vector>

int main() {
    std::unique_ptr<int> p(new int(89));
    std::vector<decltype(p)> bob;
    bob.push_back(p);
    return *p;
}
