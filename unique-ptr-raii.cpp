#include <memory>
#include <cstdlib>
#include <cstdio>

using std::puts;
using std::unique_ptr;

/* c-style alloc/free */

struct Object {
    char memory;
};

Object *Object_new() {
    Object *rv = static_cast<Object*>(malloc(sizeof(Object)));
    puts("Object_new");
    return rv;
}

void Object_free(Object *obj) {
    if (obj) { free(obj); }
}

/* wrap above into unique_ptr */

struct ObjectDeleter {
    void operator () (Object *obj) {
        puts("ObjectDeleter::operator()");
        if (obj) { free(obj); }
    }
};
using Objptr = std::unique_ptr<Object, ObjectDeleter>;

int main() {
    Objptr obj(Object_new());
    return 0;
}
