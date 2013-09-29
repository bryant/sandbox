#include <iostream>
#include <vector>
#include <numeric>

template <typename Floating>
Floating variance(const std::vector<Floating> &samples) {
    using namespace std;
    Floating mean(accumulate(samples.begin(), samples.end(), Floating(0)) /
                  samples.size());
    cout << mean << endl;
    Floating variance(0);
    for (auto &i: samples) {
        variance += (i-mean) * (i-mean);
    }
    return variance / (samples.size()-1);
}

int main() {
    using namespace std;
    vector<double> samples = {1, 2, 3, 44.5};
    for (auto &k: samples) {
        cout << k << endl;
    }
    cout << variance<double>(samples) << endl;
}
