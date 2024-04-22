/// base runtime for object
#include <tapestry/object.hpp>

u64 fnv1a_hash(const void* data, size_t length, u64 hash) {
    const u8* bytes = (const u8*)data;
    for (size_t i = 0; i < length; ++i) {
        hash ^= bytes[i];  // xor bottom with current byte
        hash *= FNV_PRIME; // multiply by FNV prime
    }
    return hash;
}

type** object::types;

object* object::hold() {
    refs++;
    return this;
}
object::object() {
    refs = 1;
}

void object::drop() {
    if (--refs == 0)
        delete this;
}

object::object(type* t) : object() {
    info = t;
}


array::array():object(typeof(object, array)) {
    items = null;
    count = 0;
    alloc = 0;
    reserve(1);
}
array::array(int sz):object(typeof(object, array)) {
    assert(sz > 0);
    items = null;
    count = 0;
    alloc = 0;
    reserve(sz);
}
void array::reserve(int n) {
    object **prev = items;
    items = new object*[n];
    assert(n >= count);
    if (prev && count)
        memcpy(items, prev, count * sizeof(object*));
    memset(&items[count], 0, (n - count) * sizeof(object*));
    alloc = n;
}
void array::push(object *o) {
    if (count == alloc)
        reserve(alloc << 1);
    items[count++] = o;
}
object* array::pop() {
    assert(count > 0);
    object *o = items[count - 1];
    items[count--] = null;
    return o;
}
object* array::shift() {
    object *o = items[0];
    for (int i = 1; i < count; i++)
        items[i - 1] = items[i];
    return o;
}


prop::prop():object(typeof(object, prop)) {
    member = null;
    info   = null;
    offset = 0;
    ptr    = null;
}
prop::prop(symbol member, void* ptr, type* info) : object(typeof(object, prop)), member(member), ptr(ptr), info(info) { }


array *object::meta() {
    return new array(1);
}

u64 object::hash_value() {
    assert(info->meta);
    assert(info->meta->count);
    u64 hash = OFFSET_BASIS;
    for (int i = 0; i < info->meta->count; i++) {
        prop* pr  = (prop*)info->meta->items[i];
        void* ptr = &((u8*)this)[pr->offset];
        if (pr->ob) {
            u64 h = ((object*)ptr)->hash_value();
            for (int i = 0; i < 8; i++) {
                hash ^= ((u8*)&h)[i];
                hash *= FNV_PRIME;
            }
        } else {
            hash = fnv1a_hash(ptr, pr->info->sz, hash);
        }
    }
    return hash;
}



str::str():object(typeof(object, str)) {
    chars = null;
    alloc = 1;
    count = 0;
    reserve(64);
    chars[0] = 0;
}

u64 str::djb2(cstr str) {
    u64     h = 5381;
    u64     v;
    while ((v = *str++))
            h = h * 33 + v;
    return  h;
}

void str::reserve(int n) {
    char *prev = chars;
    chars = new char[n];
    assert(n >= count);
    if (prev && count)
        memcpy(chars, prev, count * sizeof(char));
    memset(&chars[count], 0, (n - count) * sizeof(char));
    alloc = n;
}

void str::append(char* data, int len) {
    if (count + len >= alloc)
        reserve((count + len) * 2 + 32);
    memcpy(&chars[count], data, len);
    count += len;
    chars[count] = 0;
}

u64 str::hash_value() {
    return djb2(chars);
}


field::field(object* key, object* val, u64 hash) : object(), key(key), val(val), hash(hash) { }
field::field() : object(), key(null), val(null), hash(0) { }


map::map(int sz):object(typeof(object, map)) {
    fields = new array(sz);
}

void map::set(object* key, object* val) {
    u64    h = key->hash_value();
    field* f = new field(key->hold(), val->hold(), h);
    fields->push(f);
}
object *map::get(object* key) {
    return null;
}


type::type(symbol name, int sz, object *o) : object() {
    if (!o) {
        meta     = new array(1);
        prop* pr = new prop();
        pr->info = this;
        meta->items[0] = pr; // leave offset as 0
    } else {
        meta = o->meta();
        for (int i = 0; i < meta->count; i++) {
            prop*   pr = (prop*)meta->items[i];
            pr->offset = (u8*)pr->ptr - (u8*)o;
        }
    }
}


test::test():object(typeof(object, test)) { }

array* test::meta() {
    array *res = new array(2);
    res->push(new prop("member1", &member1, typeof(object, i32)));
    res->push(new prop("member2", &member2, typeof(object, i32)));
    return res;
}

/// pattern: module::init 
void object::init() {
    object::types = (type**)calloc(64, sizeof(type*));
    object::types[id::boolean] = new type { "bool", sizeof(bool) };
    object::types[id::u8]      = new type { "u8",   sizeof(u8)   };
    object::types[id::i8]      = new type { "i8",   sizeof(i8)   };
    object::types[id::u16]     = new type { "u16",  sizeof(u16)  };
    object::types[id::i16]     = new type { "i16",  sizeof(i16)  };
    object::types[id::u32]     = new type { "u32",  sizeof(u32)  };
    object::types[id::i32]     = new type { "i32",  sizeof(i32)  };
    object::types[id::u64]     = new type { "u64",  sizeof(u64)  };
    object::types[id::i64]     = new type { "i64",  sizeof(i64)  };
    
    object::types[id::array]   = new type { "array", sizeof(array), new array() };
    object::types[id::str]     = new type { "str",   sizeof(str),   new str()   };
    object::types[id::prop]    = new type { "prop",  sizeof(prop),  new prop()  };
    object::types[id::field]   = new type { "field", sizeof(field), new field() };
    object::types[id::map]     = new type { "map",   sizeof(map),   new map()   };

    object::types[id::test]    = new type { "test",  sizeof(test),  new test()  }; // one entry per user type
}

static object mod;