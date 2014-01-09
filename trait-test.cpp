struct A { const bool b() const { return true; } };
struct B { bool c; };

template<typename T>
void f(T&& t) {
    t.b();
}

int main() {
    f(A());
    f(B());
    return 0;
}
