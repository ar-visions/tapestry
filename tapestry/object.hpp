#pragma once
#include <stdio.h>
#include <memory.h>
#include <assert.h>
#include <cstdint>
#include <cstddef>
#include <cmath>

using null_t = nullptr_t;
static const null_t null = nullptr;

using  symbol = const char *;
using    cstr = char *;
using boolean = bool;
using      u8 = uint8_t;
using      i8 = int8_t;
using     u16 = uint16_t;
using     i16 = int16_t;
using     u32 = uint32_t;
using     i32 = int32_t;
using     u64 = uint64_t;
using     i64 = int64_t;
using     r32 = float;
using     r64 = double;

#define typeof(mod, t) mod::types[mod::id::t]

// FNV-1a constants for 64-bit hash
static constexpr uint64_t FNV_PRIME    = 0x100000001b3;
static constexpr uint64_t OFFSET_BASIS = 0xcbf29ce484222325;

u64 fnv1a_hash(const void* data, size_t length, u64 hash = OFFSET_BASIS);

struct object {
    struct type* info;
    int   refs;

    struct id {
        enum {
            boolean,
            u8,
            i8,
            u16,
            i16,
            u32,
            i32,
            u64,
            i64,
            r32,
            r64,
            array,
            str,
            prop,
            field,
            map,
            test
        };
    };
    static struct type** types;
    static void init();

    object* hold();
    object();
    object(struct type*);
    void drop();
    virtual struct array* meta();
    virtual u64 hash_value();
};

struct type:object {
    struct type*  parent;
    symbol        name;
    int           sz;
    struct array* meta;
    type(symbol name, int sz, struct object* o = null);
};

struct array:object {
    object **items;
    int count;
    int alloc;

    array();
    array(int sz);
    void reserve(int n);
    void push(object *o);
    object* pop();
    object* shift();
};

struct prop:object {
    symbol        member;
    struct type*  info;
    int           offset;
    void*         ptr;
    object*          ob;
    prop();
    prop(symbol member, void* ptr, type* info);
};

struct str:object {
    char* chars;
    int   count;
    int   alloc;

    str();
    static u64 djb2(cstr str);
    void reserve(int n);
    void append(char* data, int len);
    u64 hash_value() override;
};

struct field:object {
    object* key;
    object* val;
    u64  hash;
    field(object* key, object* val, u64 hash);
    field();
};

struct map:object {
    array* fields;

    map(int sz = 1);
    void set(object* key, object* val);
    object *get(object* key);
};

struct test:object {
    i32 member1;
    i32 member2;

    test();
    array* meta() override;
};
