#include <tapestry/tapestry.hpp>

/// use mx internally
#include <mx/mx.hpp>

namespace ion {

struct Test {
    str method1(int arg, str arg2) {
        console.log("arg = {0}, arg2 = {1}", { arg, arg2 });
        return "test";
    }
    properties meta() {
        return {
            prop { "method1", &Test::method1, this }
        };
    }
};

struct Ident:mx {
    struct M {
        Array<str> strings;
        int compare(Ident::M &b) {
            bool same;
            if (b.strings.len() == strings.len()) {
                int i = 0;
                same = true;
                for (str &s: strings) {
                    if (s != b.strings[i++]) {
                        same = false;
                        break;
                    }
                }
            } else
                same = false;
            return same ? 0 : -1;
        }
        u64 hash() const {
            u64 h = OFFSET_BASIS;
            for (str &fs: strings) {
                h *= FNV_PRIME;
                h ^= fs.hash();
            }
            return h;
        }
    };
    Ident(str s) : Ident() {
        data->strings = Array<str> { s };
    }
    Ident(Array<str> strings) : Ident() {
        data->strings = strings;
    }
    bool operator==(const Ident &s) const {
        return data->strings == s->strings;
    }
    bool operator!=(const Ident &s) const {
        return !operator==(s);
    }
    bool operator==(const str &s) const {
        if (len() == 1 && data->strings[0] == s)
            return true;
        return false;
    }
    str &operator[](int i) const {
        return data->strings[i];
    }
    sz_t len() const {
        return data->strings.len();
    }
    mx_basic(Ident);
};

static Array<str> keywords = { "method", "import" };

struct Line {
    Array<Ident> tokens;
    int indent;
    int num;
    
    int find(str token) {
        int i = 0;
        for (Ident &t: tokens) {
            if (t == token)
                return i;
            i++;
        }
        return -1; 
    }
    static int indent_count(char *&cur) {
        int count = 0;
        while (*cur == '\t') {
            count++;
            cur++;
        }
        return count;
    }
    static void ws(char *&cur) {
        while (*cur == ' ' || *cur == '\r' || *cur == '\t') ++cur;
    }

    static Ident parse_token(char *start, sz_t len) {
        while (start[len - 1] == '\t' || start[len - 1] == ' ')
            len--;
        str all { start, len };
        char t = all[0];
        return (t == '-' || (t >= '0' && t <= '9')) ? Ident(all) : Ident(all.split(".")); /// mr b: im a token!
    }

    static Array<Line> read_tokens(str input) {
        str         sp       = "$()![]+-*/:\"\'#"; /// needs string logic in here to make a token out of the entire "string inner part" without the quotes; those will be tokens neighboring
        char        until    = 0; /// either ) for $(script) ", ', f or i
        sz_t        len      = input.count();
        Array<Line> res;
        Array<Ident> tokens;
        int         indents  = 0;
        char*       origin   = input.data;
        char*       start    = 0;
        char*       cur      = origin - 1;
        int         line_num = 0;
        bool        new_line = true;
        bool        token_type = false;
        ///
        while (*(++cur)) {
            if (!until) {
                if (new_line) {
                    indents = indent_count(cur);
                    new_line = false;
                }
                ws(cur);
            }
            if (!*cur) break;
            bool add_str = false;
            char *pcur = cur; /// we adjust based on comment, and need this saved so we know where the start of the comment is before we consume the line
            
            if (*pcur == '#') { // comment
                while (*pcur && *pcur != '\n')
                    pcur++;
                new_line = true;
            }
            
            if (until) {
                if (*pcur == until && *(pcur - 1) != '/') {
                    add_str = true;
                    until = 0;
                }
            }
            if (!until) {
                int type = sp.index_of(*pcur);
                new_line |= *pcur == '\n';

                if (start && (add_str || (token_type != (type >= 0)) || new_line)) {
                    tokens += parse_token(start, (sz_t)(pcur - start));
                    if (*pcur == '+') {
                        int test = 0;
                    }
                    if (!add_str) {
                        if (*pcur == '$' && *(pcur + 1) == '(') // shell
                            until = ')';
                        else if (*pcur == '"') // double-quote
                            until = '"';
                        else if (*pcur == '\'') // single-quote
                            until = '\'';
                    }

                    if (new_line) {
                        start = null;
                    } else {
                        start = pcur;
                        token_type = (type >= 0);
                    }
                }
                else if (!start && !new_line) {
                    start = pcur;
                    token_type = (type >= 0);
                }
            }
            if (new_line) {
                if (tokens) {
                    res += Line { .tokens = tokens, .indent = indents, .num = line_num };
                    tokens = {};
                }
                until = 0;
                line_num++;
            }
        }
        if (tokens)
            res += Line { .tokens = tokens, .indent = indents, .num = line_num };
        return res;
    }
};

struct Op:mx {
    enums(Type, Undefined,
        Undefined, LiteralReal, LiteralInt, LiteralStr, Array, Var, Add, Sub, Mul, Div, Or, And, Xor, MethodDef, MethodCall, MethodReturn)
    
    struct M {
        Type        type;
        var         value;
        Array<Op>   operands;
    };

    static Op make(Type type, Array<Op> operands) {
        Op op;
        op->type = type;
        op->operands = operands;
        return op;
    }

    static Op value(Type type, mx value) {
        Op op;
        op->type = type;
        op->value = value;
        return op;
    }

    static memory *lookup(const Array<map> &stack, Ident ident, bool top_only) {
        for (int i = stack.len() - 1; i >= 0; i--) {
            map &m = stack[i];
            field *f = m->lookup(ident);
            if (f)
                return f->value.mem;
            if (top_only)
                break;
        }
        return null;
    }
    
    static var exec(const Op &op, const Array<map> &stack, bool k, int r) {
        switch (op->type) {
            case Type::LiteralInt:
            case Type::LiteralReal:
            case Type::LiteralStr:
                return op->value;
            case Type::Array: {
                array res(typeof(var), op->operands.len());
                for (Op operand: op->operands.elements<Op>()) {
                    var v = exec(operand, stack, k, r + 1);
                    res.push<var>(v);
                }
                return res;
            }
            case Type::Var: {
                bool is_isolate = (k && r == 0); /// key-mode, and the variable is isolated; that means we dont perform a depth search
                assert(op->value.type() == typeof(Ident));
                memory *m = lookup(stack, op->value, is_isolate);
                if (m)
                    return is_isolate ? var(op->value) : var(hold(m)); /// in key results, we return its field
                if (is_isolate) {
                    mx new_value;
                    map &top = stack[stack.count() - 1];
                    top[op->value] = new_value;
                    return var(op->value); /// new value is made on stack
                }
                console.fault("var not found from field: {0}", { op->value });
                break;
            }
            case Type::Add: return exec(op->operands[0], stack, k, r + 1) +  exec(op->operands[1], stack, k, r + 1);
            case Type::Sub: return exec(op->operands[0], stack, k, r + 1) -  exec(op->operands[1], stack, k, r + 1);
            case Type::Mul: return exec(op->operands[0], stack, k, r + 1) *  exec(op->operands[1], stack, k, r + 1);
            case Type::Div: return exec(op->operands[0], stack, k, r + 1) /  exec(op->operands[1], stack, k, r + 1);
            case Type::And: return exec(op->operands[0], stack, k, r + 1) && exec(op->operands[1], stack, k, r + 1);
            case Type::Or:  return exec(op->operands[0], stack, k, r + 1) || exec(op->operands[1], stack, k, r + 1);
            case Type::Xor: return exec(op->operands[0], stack, k, r + 1) ^  exec(op->operands[1], stack, k, r + 1);
            default:
                break;
        }
        return null;
    }

    mx_basic(Op)
};

struct Descent {
    Array<type_t> types = { typeof(path) };    
    Array<Ident>  tokens;
    int           count;
    int           index;
    
    Descent(Array<Ident> &a_tokens, int from, int count) : tokens(sz_t(count)), index(0), count(count) {
        int ii = 0;
        for (int i = from; i < from + count; i++)
            tokens += a_tokens[i];
        tokens += Ident {};
    }
    
    bool expect(const Ident &token) {
        assert(index < count);
        if (tokens[index] != token)
            console.fault("expected token: {0}", {token});
        return true;
    }

    Op::Type is_numeric() {
        Ident &token = tokens[index];
        if (token.len() > 1)
            return Op::Type::Undefined;
        char t = tokens[index][0][0];
        return (t >= '0' && t <= '9') ? (strchr(token[0].data, '.') ? Op::Type::LiteralReal : Op::Type::LiteralInt) : Op::Type::Undefined;
    }

    Op::Type is_var() { /// type, var, or method (all the same); its a name that isnt a keyword
        Ident &token = tokens[index];
        char t = token[0][0];
        if (isalpha(t) && keywords.index_of(token[0]) == -1) {
            /// lookup against variable table; declare if in isolation
            return Op::Type::Var;
        }
        return Op::Type::Undefined;
    }

    Ident consume(const Ident &token) {
        expect(token);
        return tokens[index++];
    }

    Ident consume() {
        assert(index < count);
        return tokens[index++];
    }

    Ident &next() {
        Ident &nx = tokens[index];
        return tokens[index];
    }

    Op parse_add() {
        Op left = parse_mult();
        while (next() == "+" || next() == "/") {
            Op::Type type = next() == "+" ? Op::Type::Add : Op::Type::Sub;
            consume();
            Op right = parse_mult();
            left = Op::make(type, Array<Op> { left, right });
        }
        return left;
    }

    Op parse_mult() {
        Op left = parse_primary();
        while (next() == "*" || next() == "/") {
            Op::Type type = next() == "*" ? Op::Type::Mul : Op::Type::Div;
            consume();
            Op right = parse_primary();
            left = Op::make(type, Array<Op> { left, right });
        }
        return left;
    }

    Op parse_primary() {
        Op::Type n = is_numeric();
        if (n) {
            Ident &f = next();
            assert(f.len() == 1);
            cstr cs = f[0].data;
            bool is_int = n == Op::Type::LiteralInt;
            consume();
            return Op::value(n, mx::from_string(cs, is_int ? typeof(i64) : typeof(double)));
        }
        Op::Type i = is_var(); /// its a variable or method (same thing; methods consume optional args)
        if (i) {
            Ident &f = next();
            consume();
            return Op::value(i, f); /// the entire Ident is value
        }
        if (next() == "(") {
            consume();
            Op op = parse();
            expect(str(")"));
            consume();
            return op;
        }
        return {};
    }

    Op parse() {
        return parse_add();
    }
};

struct Expression:mx {
    struct M {
        Op key;
        Op val;
        str text;
        Array<Expression> children;
    };
    
    static Op parse_ops(Array<Ident> &tokens, int start, int count) {
        Descent des { tokens, start, count };
        return des.parse();
    }

    static Array<Expression> parse(const path& ta) {
        str         contents = ta.read<str>();
        Array<Line> lines = Line::read_tokens(contents);
        Array<Expression> res;
        Array<Array<Expression>*> parents = { &res };
        ///
        if (!lines) {
            console.fault("tapestry module empty: {0}", { str(ta) });
        } else {
            Expression  cur;
            Expression  prev;
            int         prev_indent = 0;
            int         line_num = 0;
            for (Line &line: lines.elements<Line>()) {
                int diff = line.indent - prev_indent;
                if (diff > 0) {
                    if (diff != 1) {
                        console.fault("too many indents at {0}:{1}", { ta, line.num });
                        res.clear();
                        break;
                    }
                    parents.push(&cur->children);
                } else if (diff < 0) {
                    for (int i = 0; i < -diff; i++)
                        parents.pop<Array<Expression>*>();
                }
                cur = Expression { };
                *parents[parents.count() - 1] += cur;

                /// read line tokens
                int col         = line.find(":");
                int key_ops     = col == -1 ? line.tokens.len() : col;
                int val_ops     = col == -1 ? 0 : (line.tokens.len() - (col + 1));

                assert(key_ops);
                if (key_ops) {
                    Ident f = line.tokens[0];
                    cur->key = parse_ops(line.tokens, 0, key_ops);
                }
                if (val_ops)
                    cur->val = parse_ops(line.tokens, key_ops + 1, val_ops);
                prev = cur;
            }
        }
        return res;
    }
    static void exec_expressions(Array<Expression> &expr, Array<map> &stack) {
        map &top = stack.push<map>();
        for (Expression &e: expr) {
            /// the val needs to exec first, because it may refine a variable
            var val = Op::exec(e->val, stack, false, 0);
            var key = Op::exec(e->key, stack, true,  0);
            if (key.type() == typeof(Ident)) { /// set variable to value
                top[key] = val; /// val is null and should be i64:1
                if (e->children)
                    exec_expressions(e->children, stack);
            } else if (key.type() == typeof(Array<mx>)) { /// elements loop
                assert(val.type() == typeof(Ident));
                if (e->children) {
                    Array<mx> a(hold(key.mem));
                    for (mx &element: a) {
                        top[val] = element; /// each iteration, assign the element to this Ident
                        exec_expressions(e->children, stack);
                    }
                }
            } else if (bool(key)) { /// if statement; a while can be   while: a < 2
                /// a: a + 1
                if (e->children)
                    exec_expressions(e->children, stack);
            }
        }
        //stack.pop<map>();
    }
    static void exec(Array<Expression> &expr) {
        Array<map> stack(64);
        exec_expressions(expr, stack);
        for (map &m: stack) {
            for (field &f: m.fields()) {
                console.log("{0}: {1}", { f.key, f.value });
            }
        }
    }
    mx_basic(Expression)
};

}

using namespace ion;

#define typeOf(T)                      (id::for_type<T>())
#define signatureOf(CL,M,MPTR,MPTR_SZ) (id::for_type<M,CL>((void*)MPTR,MPTR_SZ))

u64 fnv1a_hash(const void* data, size_t length, u64 hash = OFFSET_BASIS);

u64 fnv1a_hash(const void* data, size_t length, u64 hash) {
    const u8* bytes = (const u8*)data;
    for (size_t i = 0; i < length; ++i) {
        hash ^= bytes[i];  // xor bottom with current byte
        hash *= FNV_PRIME; // multiply by FNV prime
    }
    return hash;
}

struct method_info {
    struct id** args;
    struct id*  r_type;
    int arg_count;
    std::function<memory*(void*, memory**, int)> call; /// for methods
};

struct id {
    struct id       *src;     ///
    cstr             name;
    size_t           base_sz; /// a types base sz is without regards to pointer state
    size_t           traits;
    bool             pointer; /// allocate enough space for a pointer to be stored
    struct List     *meta;    /// properties
    struct Hash     *meta_map; // prop_map
    method_info     *method;

    id() { }

    id(bool);

    static inline struct ahashmap *types;
    static inline id *char_t; /// cannot use hashmap/mx::hash without a registered char type
    static inline id *i64_t;
    static inline id *u64_t;

    struct f_table {
        std::function<void(void*)>         dtr;
        std::function<void(void*)>         ctr;
        std::function<void(void*,struct A*)>    ctr_mem;  /// for mx objects (assign from memory)
        std::function<void(void*,const struct str2 &)> ctr_str; /// for mx objects (copy convert)
        std::function<void(void*,void*)>   ctr_cp;
        std::function<void(void*,void*,void*,double)> mix;
        std::function<bool(void*)>         boolean;
        std::function<u64(void*)>          hash;
    } f;

    struct symbols2 *symbols;
    id              *ref; /// if you are given a primitive enum, you can find the schema and symbols on ref (see: to_string)
    id              *parent; 
    bool             secondary_init; /// used by enums but flag can be used elsewhere.  could use 'flags' too

    void   *alloc();
    void   *ctr();
    void    dtr(void* alloc);
    void   *ctr_mem (A *mem);
    void   *ctr_str (const struct str2 &v);
    void   *ctr_cp  (void* b);
    size_t  data_size();
    template <typename TT, typename CL=none>
    static id *for_type(void *MPTR=nullptr, size_t MPTR_SZ=0);
    static void init();
};

template <> id *id::for_type<null_t>(void*, size_t);
template <> id *id::for_type<char>  (void*, size_t);
template <> id *id::for_type<i64>   (void*, size_t);
template <> id *id::for_type<u64>   (void*, size_t);

static cstr parse_fn(const std::string &cn);

static void describe_type(memory *mem, cstr name, sz_t sz, u64 traits);

static void push_type(id *type);

void register_type2(id *type, const std::string &sig, sz_t sz, u64 traits);

/// these are called before registration, by ident::init
static id *primitive_type(symbol name, sz_t sz);

template <>
id *id::for_type<null_t>(void*, size_t) {
    static id* type; if (type) return type;
    type = primitive_type("std::nullptr_t", sizeof(null_t));
    return type;
}

template <>
id *id::for_type<char>(void*, sz_t) {
    static id* type; if (type) return type;
    type = primitive_type("char", sizeof(char));
    type->traits |= traits::integral;
    return type;
}

template <>
id *id::for_type<i64>(void*, sz_t) {
    static id* type; if (type) return type;
    type = primitive_type("long long", sizeof(i64));
    type->traits |= traits::integral;
    return type;
}

template <>
id *id::for_type<u64>(void*, sz_t) {
    static id* type; if (type) return type;
    type = primitive_type("unsigned long long", sizeof(u64));
    type->traits |= traits::integral;
    return type;
}

/// now that we have allowed for any, make entry for meta description
struct iprop {
    struct m *key;
    size_t  member_addr;
    size_t  offset; /// this is set by once-per-type meta fetcher
    id*     type;
    id*     parent_type;
    raw_t   init_value; /// value obtained after constructed on the parent type
    bool    is_method; /// make sure we dont enumerate the data on methods

    iprop() ;

    template <typename M>
    iprop(symbol name, const M &member) ;
    
    template <typename M, typename CL>
    iprop(symbol name, const M &member, const CL* inst) ;

    template <typename M>
    M &member_ref(void *m) ;

    void *member_pointer(void *m) ;

    symbol name() const ;
};

/// we cannot actually move these around so easy without a copy
/// all ax-classes would need to be contained in pointer containment
/// we would never actually create anything 'ax' or sub derived without a new keyword, must be stack
/// if by hack we can enforce this

/// we cannot ever put an 'A' class on stack, as we do with mx currently
/// that could mean, however, mx could be a user of A
struct A { /// runtime of silver-lang
    id*     type;
    u64     h;
    int     refs; /// no schema, since we'll use traditional c++ 17
    A() : type(null), refs(1) { }
    A   *hold() { refs++; return this; }
    void drop() { if (--refs <= 0) { printf("deleting object of type: %s\n", type->name); delete this; } }
    virtual u64 hash() {
        console.fault("implement hash() for type {0}", { str(type->name) });
        return 0;
    }
    virtual int compare(const struct m &ref) const {
        console.fault("implement compare() for type {0}", { str(type->name) });
        return -1;
    }
    virtual struct string *to_string() const {
        console.fault("implement to_string() for type {0}", { str(type->name) });
        return null;
    }
    virtual ~A() { }
};

struct a_user:A {
    int a;
    int b;
    operator bool() const { return true; }
};

template <typename T>
struct Value:A {
    T value;
    Value(const T& v) : value(v) { }
    u64 hash() {
        if (A::h)
            return A::h;
        id* type = typeOf(T);
        if (type->traits & traits::primitive)
            return (A::h = fnv1a_hash(&value, sizeof(T)));
        if constexpr (ion::inherits<A, T>()) /// is also in traits (or will be)
            return (A::h = value->hash());
        console.fault("implement hash for non-A-type");
        return 0;
    }
    operator bool() const { return bool(value); }
};

/// always working on string theory..
struct string:A {
    char *chars;
    int   len;
    int   alloc;

    string() : A() {
        chars = new char[64];
        alloc = 64;
        len   = 0;
        chars[0] = 0;
    }

    string(const char *v, int v_len = -1) : A() {
        len   = v_len == -1 ? strlen(v) : v_len;
        alloc = 64;
        while (alloc < len)
            alloc <<= 1;
        chars = new char[64];
        memcpy(chars, v, len);
        chars[len] = 0;
    }

    string(char *v, int v_len = -1) : string((const char *)v, v_len) { }

    string *mid(int start, int slen) {
        if (start < 0)
            start = start + len;
        return new string(&chars[start], slen);
    }

    string *trim() {
        int offset = 0;
        while (chars[offset] == ' ') {
            offset++;
        }
        int slen = len - offset;
        for (;slen;slen--) {
            if (chars[offset + slen - 1] != ' ')
                break;
        }
        return new string(&chars[offset], slen);
    }

    void append(const char* v, int append_len = -1) {
        if (append_len == -1)
            append_len = strlen(v);
        if (alloc <= len - append_len) {
            alloc = math::max(128, alloc << 1);
            char *n = new char[alloc];
            memcpy(n, chars, len);
            memcpy(&n[len], &v, append_len);
            n[len += append_len] = 0;
            delete[] chars;
            chars = n;
        } else {
            memcpy(&chars[len], v, append_len);
            chars[len += append_len] = 0;
        }
    }

    void append(char v)                { append(&v, 1); }
    void append(const string* v)       { append(v->chars, v->len); }
    void operator += (char          v) { append(&v, 1); }
    void operator += (const char*   v) { append(v, strlen(v)); }
    void operator += (const string* v) { append(v->chars, v->len); }

    u64 hash() {
        A::h = fnv1a_hash(chars, len);
        return A::h;
    }

    int compare(const m &b) const override;

    operator bool() const { return len > 0; }
};

/// these can be managed or unmanaged
struct Pointer:A {
    id   *type;
    void *ptr;
    bool  managed;
    Pointer(id* type, void *ptr, bool managed) : type(type), ptr(ptr), managed(managed) { }
    ~Pointer() {
        if (managed) free(ptr);
    }
};

struct m {
    union {
        Value<u64>  *v_u64;
        Value<u32>  *v_u32;
        Value<u16>  *v_u16;
        Value<u8>   *v_u8;
        Value<i64>  *v_i64;
        Value<i32>  *v_i32;
        Value<i16>  *v_i16;
        Value<i8>   *v_i8;
        Value<bool> *v_bool;
        Pointer     *v_pointer;
        string      *a_str;
        A           *a;
    };
    m()           : a(null) { }
    m(null_t)     : m()     { }
    m(u64      i);
    m(i64      i);
    m(u32      i);
    m(i32      i);
    m(u16      i);
    m(i16      i);
    m(u8       i);
    m(i8       i);
    m(char     i);
    m(symbol sym);
    m(cstr   str) : m((symbol)str) { }
    m(A       *a) : a(a ? a->hold() : null) { } /// this lets us pass around actual dynamically allocated A classes
    m(const m& b) : a(b.a ? b.a->hold() : null) { }
    template <typename T>
    static m pointer(T *ptr, bool managed = false) {
        m result;
        result.v_pointer = new Pointer(typeOf(T), (void*)ptr, managed);
        return result;
    }
    m& operator=(const m &b) {
        if (a != b.a) {
            if (a) a->drop();
            a = b.a->hold();
        }
        return *this;
    }
    u64 hash() {
        return a ? a->hash() : 0;
    }
    ~m() { if (a) a->drop(); }

    bool operator==(const m &ref) const {
        return compare(ref) == 0;
    }

    int compare(const m &ref) const {
        if (a == ref.a) return true;
        if (a->type != ref.a->type) return false;
        return a->compare(ref);
    }
    
    operator bool() const { return (a && a->type->f.boolean) ? a->type->f.boolean(a) : false; }
    
    template <typename T>
    T* get() const {
        //static id* type_check = typeOf(T);
        //assert(type_check == a->type);
        return (T*)a;
    }

    operator symbol() {
        //assert(a->type == typeOf(str2));
        return a_str ? symbol(a_str->chars) : symbol(null);
    }
};

iprop::iprop() : key(null), member_addr(0), offset(0), type(null), is_method(false) { }

template <typename M>
iprop::iprop(symbol name, const M &member) : key(new m(name)), member_addr((size_t)&member), type(typeOf(M)), is_method(false) { }

template <typename M, typename CL>
iprop::iprop(symbol name, const M &member, const CL* inst) : 
    key(new m(name)), member_addr((size_t)&member), type(signatureOf(CL, M, member_addr, sizeof(member))), is_method(true) { }

template <typename M>
M &iprop::member_ref(void *m) { return *(M *)handle_t(&cstr(m)[offset]); }

void *iprop::member_pointer(void *m) { return (void *)handle_t(&cstr(m)[offset]); }

symbol iprop::name() const { return symbol(*key); }

int string::compare(const m &arg) const {
    string *b = arg.get<string>();
    return strcmp(chars, b->chars);
}

template <typename T> struct is_vector : false_type {};

struct ilist:A {
    protected:
    int count;
    public:
    ilist() { count = 0; }
    virtual void remove(int index, int amount = 1) = 0;
    virtual void push(const m &v) = 0;
    virtual m pop() = 0;
    virtual void clear() = 0;
};

template <typename T>
struct vector:ilist {
    private:
    T  *elements;
    int alloc;
    int count;

    public:
    vector() : ilist() {
        alloc = 0;
        resize(32);
    }

    vector(std::initializer_list<T> list) : vector() {
        for (auto i: list)
            push(i);
    }

    void remove(int index, int amount = 1) {
        assert(count >= (index + amount));

        int w = index;
        for (int i = index + amount; i < count; i++)
            elements[w++] = elements[i];

        while (w < count)
            elements[w++].~T();

        count -= amount;
    }

    void push(const T &v) {
        if (alloc == count)
            resize(alloc << 1);
        
        new (&elements[count]) T(v);
    }

    T pop() {
        assert(count > 0);
        T res = elements[count];
        elements[--count].~T();
        return res;
    }
    
    void operator += (const T &v) {
        push(v);
    }

    void clear() {
        int w = 0;
        while (w < count)
            elements[w++].~T();
        count = 0;
    }

    void resize(int sz) {
        if (sz == alloc) return;
        assert(sz >= count);
        alloc = sz;
        T *n = (T*)calloc(alloc, sizeof(T));
        for (int i = 0; i < count; i++)
            new (&n[i]) T(*(const T*)&elements[i]);
        free(elements);
        elements = n;
    }

    u64 hash() {
        bool is_prim = type->traits & traits::primitive;
        if (A::h) return A::h;
        u64 h = OFFSET_BASIS;
        for (int i = 0; i < count; i++) {
            h *= FNV_PRIME;
            h ^= is_prim ? fnv1a_hash(&elements[i], type->base_sz) : ((A*)&elements[i])->hash();
        }
        A::h = h;
        return h;
    }

    operator bool() const { return count > 0; }
};

using arr = vector<m>;

template <typename T> struct is_vector<vector<T>> : true_type { };

struct aitem:A {
    friend struct alist;
    private:
    struct aitem* next;
    struct aitem* prev;
    public:
    m element; /// mx is still the intermediate class, since it holds onto and can debug any of the data you give it
    aitem() : A() {
        next = 0;
        prev = 0;
    }
    aitem(const m &e) : aitem() {
        element = (m&)e;
    }
    u64 hash() {
        return element.hash();
    }
    operator bool() { return element.a; }
};

struct alist:ilist {
    aitem* first;
    aitem* last;
    alist() : ilist() {
        first = null;
        last  = null;
        count = 0;
    }
    template <typename T>
    alist(std::initializer_list<T> list) : alist() {
        for (auto i: list)
            push(i);
    }
    void clear() {
        while (last)
            remove(last);
    }
    void remove(aitem *i) {
        aitem *inext = i->next;
        aitem *iprev = i->prev;
        if (inext) inext->prev = iprev;
        if (iprev) iprev->next = inext;
        if (first == i) first  = inext;
        if (last  == i) last   = iprev;
        delete i;
        count--;
    }
    void remove(int index, int amount = 1) {
        int f = 0;
        bool found = false;
        for (aitem *i = first; i; i = i->next) {
            if (index == f++) {
                for (int ii = 0; ii < amount; ii++) {
                    assert(i);
                    aitem *inext = i->next;
                    remove(i);
                    i = inext;
                }
                found = true;
                break;
            }
        }
        assert(found);
    }
    void push(const m &v) {
        aitem *i = new aitem(v);
        if (last) {
            last->next = i;
            i->prev    = last;
            last       = i;
        } else
            first      = (last = i);
        count++;
    }
    m pop() {
        assert(last);
        m res = last->element;
        aitem *ilast = last;
        last  = last->prev;
        if (!last)
            first = 0;
        delete ilast;
        return res;
    }

    void operator += (const m &v) {
        push(v);
    }
    operator bool() const { return count > 0; }


    /// iterator unique for doubly
    struct liter_item {
        aitem* cur;
        liter_item(aitem *cur) : cur(cur) { }
        ///
        liter_item& operator++() { cur = cur->next; return *this; }
        liter_item& operator--() { cur = cur->prev; return *this; }

        aitem* operator *() const { return cur; }
        operator aitem *() const { return cur; }

        bool operator==  (const liter_item& b) const { return cur == b.cur; }
        bool operator!=  (const liter_item& b) const { return cur != b.cur; }
    };

    /// doubly is partnered with hashmap, so an iterator filters by hash
    struct liter_item_hash:liter_item {
        u64   hash;
        using LT = liter_item;
        liter_item_hash(aitem *cur, u64 hash) : liter_item(cur), hash(hash) {
            while (LT::cur && (LT::cur->hash() != hash))
                LT::cur = LT::cur->next;
        }

        ///
        liter_item_hash& operator++() {
            do { LT::cur = LT::cur->next; } while (LT::cur && LT::cur->hash() != hash);
            return *this;
        }
        liter_item_hash& operator--() {
            do { LT::cur = LT::cur->prev; } while (LT::cur && LT::cur->hash() != hash);
            return *this;
        }
    };

    /// better to use the .elements<T> on generics; we cannot have a begin() on something without a template arg
    template <typename T>
    struct literable {
        aitem *first, *last;
        literable(aitem *first, aitem *last) :
            first(first), last(last) { }
        liter<T> begin() const { return liter<T>{ first }; }
        liter<T>   end() const { return liter<T>{ null }; }
    };

    template <typename T>
    struct literable_hash:literable<T> {
        u64 hash;
        literable_hash(aitem *first, aitem *last, u64 hash) :
            literable<T>(first, last), hash(hash) { }
        liter_hash<T> begin() const { return liter_hash<T>{ literable<T>::first }; }
        liter_hash<T>   end() const { return liter_hash<T>{ null }; }
    };

    struct literable_items {
        aitem *first, *last;
        literable_items(aitem *first, aitem *last) : first(first), last(last) { }
        liter_item begin() const { return liter_item{ first }; }
        liter_item   end() const { return liter_item{ null }; }
    };

    struct literable_items_hash:literable_items {
        u64 hash;
        literable_items_hash(aitem *first, aitem *last, u64 hash) :
            literable_items(first, last), hash(hash) { }
        liter_item_hash begin() const { return liter_item_hash { literable_items::first, hash }; }
        liter_item_hash   end() const { return liter_item_hash { null, 0 }; }
    };

    template <typename T>
    literable<T> elements() const { return literable<T> { first, last }; }

    template <typename T>
    literable_hash<T> elements(u64 hash) const { return literable_hash<T>    { first, last, hash }; }

    literable_items      items()         const { return literable_items      { first, last }; }
    literable_items_hash items(u64 hash) const { return literable_items_hash { first, last, hash }; }
};

/// never use directly, like all A-classes
struct afield:A {
    m key;
    m value; /// these are more debuggable than simple A* pointers
    afield() : A() { }
    afield(const m& k, const m& v) : afield() {
        key   = k;
        value = v;
    }
    u64 hash() {
        return key.hash();
    }
    string *to_string();
    operator bool() const { return key.a != null; }
};

template <typename T>
struct alambda;

template <typename R, typename... Args>
struct alambda<R(Args...)>:A {
    using fdata = std::function<R(Args...)>;
    fdata *lfn;

    ~alambda() {
        delete lfn;
    }
};

template <typename> struct is_lambda2 : false_type { };

template <typename T>
struct Lambda;

template <typename R, typename... Args>
struct Lambda<R(Args...)> {
    using ltype = alambda<R(Args...)>;
    ltype *data;
    Lambda(A* mem) : data(mem ? mem->hold() : null) { assert(!data || data->type == typeOf(alambda<R(Args...)>)); }
    Lambda(ltype* mem) : Lambda((A*)mem) { }
    Lambda(const Lambda &b) : data(b.data->hold()) { }
    Lambda() : data(new alambda<R(Args...)>()) { data->type = typeOf(alambda<R(Args...)>); }
    template <typename F>
    Lambda(F&& fn);
    Lambda &operator=(const Lambda &b) {
        if (data != b.data) {
            if (data) data->drop();
            data = b.data ? b.data->hold() : null;
        }
        return *this;
    }
    R operator()(Args... args) const { return (*data->lfn)(std::forward<Args>(args)...); }
    operator bool() const { return data && data->lfn && *data->lfn; }

};

template <typename T> struct is_lambda2<Lambda<T>> : true_type { };

template <typename R, typename... Args>
template <typename F>
Lambda<R(Args...)>::Lambda(F&& fn) {
    if constexpr (ion::inherits<Lambda, F>() || is_lambda2<std::remove_reference_t<F>>::value) {
        data = fn.data->hold();
    } else {
        if constexpr (std::is_invocable_r_v<R, F, Args...>) {
            data = new alambda<R(Args...)>();
            data->lfn = new alambda<R(Args...)>::fdata(std::forward<F>(fn));
        } else {
            static_assert("F type is not a functor");
        }
    }
    data->type == typeOf(alambda<R(Args...)>);
}

template<typename T, typename = void>
struct has_ctr_str2 : std::false_type {};
template<typename T>
struct has_ctr_str2<T, std::enable_if_t<std::is_constructible<T, const str2&>::value>> : std::true_type {};



template <typename T, typename = std::void_t<>>
struct has_str2_op : std::false_type {};

template <typename T>
struct has_str2_op<T, std::void_t<decltype(static_cast<str2>(std::declval<T>()))>> : std::true_type {};

template <typename T>
constexpr bool has_str2 = has_str2_op<T>::value;



template <typename T, typename = void> struct registered_instance_meta2 : false_type { };
template <typename T>
struct registered_instance_meta2<T, std::enable_if_t<std::is_same_v<decltype(std::declval<T>().meta()), List>>> : true_type { };



/// there is ambiguity around what m arg does when its implemented on A-class
/// when there is conflict (such as aitem) we will instantiate new Aclass(m) for the Aclass use case
/// sharing use-case would be to use U(m)

/// the A class is a good solution for the rule of 3, memory sharing, and isolating dependencies
#define A_decl(U, _A_) \
    private: \
    public: \
    struct _A_* data; \
    using intern = _A_;\
    U(const m& ref) : data((_A_*)(ref.a->hold()))    { ((A*)data)->type = typeOf(_A_); } \
    U(m& ref)       : U((const m&)ref) { } \
    U(_A_* data)    : data((_A_*)data) { ((A*)data)->type = typeOf(_A_); } \
    U(const U &b); \
    U(U &b); \
    U(); \
    template <typename... Arg> \
    U(Arg&&... arg) : data(new _A_(std::forward<Arg>(arg)...)) { ((A*)data)->type = typeOf(_A_); } \
    template <typename T> U& operator *= (const T& v) { (*data) *= v; return *this; }\
    template <typename T> U& operator /= (const T& v) { (*data) /= v; return *this; }\
    template <typename T> U& operator += (const T& v) { (*data) += v; return *this; }\
    template <typename T> U& operator -= (const T& v) { (*data) -= v; return *this; }\
    template <typename T> U  operator *  (const T& v) { return (*data) * v; }\
    template <typename T> U  operator /  (const T& v) { return (*data) / v; }\
    template <typename T> U  operator +  (const T& v) { return (*data) + v; }\
    template <typename T> U  operator -  (const T& v) { return (*data) - v; }\
    explicit operator bool() const; \
    operator m() const; \
    _A_* operator->() const; \
    operator _A_*() const; \
    U &operator=(const U &b); \
   ~U(); \

#define A_impl(U, _A_) \
    U::U(const U &b)      : data((_A_*)b.data->hold()) { } \
    U::U(U &b)            : U((const U&)b) { } \
    U::U()                : data(new _A_())  { data->type = typeOf(_A_); } \
    _A_* U::operator->   () const { return data; } \
         U::operator _A_*() const { return (_A_*)(data ? data->hold() : null); } \
    U   &U::operator=(const U &b) { \
        if (data != b.data) { \
            data->drop(); \
            data = (_A_*)b.data->hold(); \
        } \
        return *this; \
    } \
    U::operator bool() const { return data ? bool(*data) : false; } \
    U::operator m() const { return m(data); } \
    U::~U() { data->drop(); } \


#define A_forward(_A_, ...)  data(new _A_(__VA_ARGS__)) { ((A*)data)->type = typeOf(_A_); }

struct AUser {
    A_decl(AUser, a_user)
};
A_impl(AUser, a_user)


struct str2 {
    A_decl(str2, string)
};
A_impl(str2, string) /// this is str


string *afield::to_string() {
    return str2("{0}, {1}");
}


struct Field {
    A_decl(Field, afield)
};
A_impl(Field, afield)


struct List {
    A_decl(List,  alist)

    template <typename T>
    List(std::initializer_list<T> list) : A_forward(alist, list)
};

A_impl(List, alist)

struct Item {
    A_decl(Item, aitem)
};
A_impl(Item, aitem)


/// type is not set on these primitive structs; we dont construct them ourselves, and the macro generally does this
/// the exception is here, in the m generics. for all uses of A-type, we set their type
m::m(u64      i) : v_u64(new Value<u64>(i))    { a->type = typeOf(Value<u64>); }
m::m(i64      i) : v_i64(new Value<i64>(i))    { a->type = typeOf(Value<i64>); }
m::m(u32      i) : v_u32(new Value<u32>(i))    { a->type = typeOf(Value<u32>); }
m::m(i32      i) : v_i32(new Value<i32>(i))    { a->type = typeOf(Value<i32>); }
m::m(u16      i) : v_u16(new Value<u16>(i))    { a->type = typeOf(Value<u16>); }
m::m(i16      i) : v_i16(new Value<i16>(i))    { a->type = typeOf(Value<i16>); }
m::m(u8       i) : v_u8 (new Value<u8 >(i))    { a->type = typeOf(Value<u8>); }
m::m(i8       i) : v_i8 (new Value<i8 >(i))    { a->type = typeOf(Value<i8>); }
m::m(char     i) : v_u8 (new Value<u8>((u8)i)) { a->type = typeOf(Value<u8>); }
m::m(symbol sym) : a_str(new string(sym))      { a->type = typeOf(string);    }

struct ahashmap:A {
    alist *h_pairs;
    size_t sz;

    static u64 hash_value(m &key) {
        return key.hash();
    }

    static u64 hash_index(m &key, size_t mod) {
        return hash_value(key) % mod;
    }

    alist &list_for_key(u64 k) {
        assert(sz > 0 && k < sz);
        return h_pairs[k];
    }

    void push(const Field &f) {
        u64 k = hash_index(f->key, sz); // if sz is 1 for non-hash use-cases, that would be a reduction
        alist &b = list_for_key(k);
        b.push(f);
    }
    
    ~ahashmap() {
        delete h_pairs;
    }

    ahashmap(int sz = 32);
    ahashmap(std::initializer_list<Field> a) : ahashmap(a.size() > 0 ? a.size() : 32) {
        for (auto &v: a)
            push(v);
    }

    aitem     *item(const m &key, alist **list = null, u64 *phash = null);
    afield   *fetch(const m &key); /// we use afield here, because fetch is internal to hash and we store afield; any user who accepts a return value can contain it in a User type
    m        &value(const m &key);
    void        set(const m &key, const m &value);

    template <typename V>
    V &get(const m &k);

    m      lookup(const m &k);
    bool contains(const m& key);
    bool   remove(const m &key);

    m &operator[](const m &key);

    size_t    len() { return sz; }
    explicit operator bool() { return sz > 0; }
};

struct symbols2 {
    ahashmap   by_name  { };
    ahashmap   by_value { };
    alist      list;
};

aitem* ahashmap::item(const m &key, alist **list, u64 *phash) {
    const u64 hash = hash_value((m&)key);
    const u64 k    = hash % sz;
    if (phash) *phash = hash;
    alist &hist = list_for_key(k);
    if (list) *list = (alist*)&hist;
    for (aitem *fi: hist.items(hash)) {
        assert(fi->hash() == hash);
        afield *f = fi->element.get<afield>();
        if (f->key == key)
            return fi;
    }
    return null;
}

m ahashmap::lookup(const m &key) {
    aitem *fi = item(key, null, null);
    if (!fi)
        return m();
    afield *f = fi->element.get<afield>();
    return f->value;
}

/// always creates a field with fetch
afield *ahashmap::fetch(const m &key) {
    alist  *list = null;
    u64     hash = 0;
    aitem  *fi   = item(key, &list, &hash);
    afield *f    = null;
    if (!fi) {
        list->push(new afield(key, null)); /// we iterate with a filter on hash id in doubly
        f = list->last->element.get<afield>();
    } else {
        f = fi->element.get<afield>();
    }
    return f;
}

m &ahashmap::value(const m &key) {
    afield *f = fetch(key);
    return f->value;
}

m &ahashmap::operator[](const m &key) {
    afield *f = fetch(key);
    return f->value;
}

void ahashmap::set(const m &key, const m &value) {
    afield *f = fetch(key);
    if (value.a != f->value.a)
        f->value = value;
}

bool ahashmap::remove(const m &key) {
    alist *list = null;
    aitem *fi   = item(key, &list);
    if (fi && list) {
        list->remove(fi);
        return true;
    }
    return false;
}

bool ahashmap::contains(const m &key) {
    return ahashmap::item(key, null);
}

ahashmap::ahashmap(int sz) : A() {
    h_pairs = new alist[sz];
    this->sz = sz;
}


struct Hash {
    A_decl(Hash, ahashmap)
    Hash(std::initializer_list<Field>);
    m &operator[](const m &key) { return (*data)[key]; }
};
A_impl(Hash, ahashmap)
Hash::Hash(std::initializer_list<Field> f) : A_forward(ahashmap, f)

void id::init() {
    static id* id_types;
    if (!id_types) id_types = new id(true);
}

template <typename TT, typename CL>
id *id::for_type(void *MPTR, size_t MPTR_SZ) {
    id::init();
    using T = pure_type<TT>;

    if constexpr (has_intern<T>()) /// just so we can do typeOf(UserType) and have it return the A-type
        return id::for_type<typename T::intern>(MPTR, MPTR_SZ);
    else {
        static id* type; if (type) return type; /// assert the name and traits are set

        /// static identity: make sure we only proceed to pure type's definition; otherwise we are redefining id*    multiple times
        /// side effect is one more call
        if constexpr (!identical<T, TT>()) return for_type<T>(); 

        type          = new id();
        memset(type, 0, sizeof(id));

        bool is_A     = ion::inherits<A, T>();

        u64 traits = (is_primitive<T> ()  ? traits::primitive : 0) |
                    (is_integral <T> ()  ? traits::integral  : 0) |
                    (is_realistic<T> ()  ? traits::realistic : 0) | // if references radioshack catalog
                    (is_array    <T> ()  ? traits::array     : 0) |
                    (is_lambda2  <T> ()  ? traits::lambda    : 0) |
                    (is_map      <T> ()  ? traits::map       : 0) |
                    (has_etype<T>::value ? traits::mx_enum   : 0) |
                    (is_A                ? traits::mx_obj    : 0);

        if constexpr (std::is_member_function_pointer<T>::value) {
            using R = typename member_fn<T>::r_type;
            type->method = new method_data {
                .args      = (id**)calloc(member_fn<T>::n_args, sizeof(id*)),
                .r_type    = typeOf(R),
                .arg_count = member_fn<T>::n_args
            };

            #define member_init(...) \
                CL *obj = (CL*)inst; \
                R (CL::*member_func)(__VA_ARGS__); \
                assert(sizeof(member_func) == MPTR_SZ); \
                memcpy((void*)&member_func, (void*)MPTR_MEM, MPTR_SZ); \
                mx result;

            #define member_call(...) \
                if constexpr (identical<R, void>()) \
                    (obj->*member_func)(__VA_ARGS__); \
                else \
                    result = (obj->*member_func)(__VA_ARGS__); \
                return hold(result.mem);
            
            u8 MPTR_MEM[1024];
            memcpy(MPTR_MEM, MPTR, MPTR_SZ);
            if constexpr (member_fn<T>::n_args == 0) {
                type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> memory* {
                    member_init()
                    member_call()
                };
            }
            else if constexpr (member_fn<T>::n_args == 1) {
                using A0 = typename member_fn<T>::argument<0>::type;
                type->method->args[0] = typeOf(A0);
                type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> memory* {
                    A0 a0 = m(args[0]);
                    member_init(A0)
                    member_call(a0)
                };
            }
            else if constexpr (member_fn<T>::n_args == 2) {
                using A0 = typename member_fn<T>::argument<0>::type;
                using A1 = typename member_fn<T>::argument<1>::type;
                type->method->args[0] = typeOf(A0);
                type->method->args[1] = typeOf(A1);
                type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> memory* {
                    A0 a0 = m(args[0]);
                    A1 a1 = m(args[1]);
                    member_init(A0, A1)
                    member_call(a0, a1)
                };
            }
            else if constexpr (member_fn<T>::n_args == 3) {
                using A0 = typename member_fn<T>::argument<0>::type;
                using A1 = typename member_fn<T>::argument<1>::type;
                using A2 = typename member_fn<T>::argument<2>::type;
                type->method->args[0] = typeOf(A0);
                type->method->args[1] = typeOf(A1);
                type->method->args[2] = typeOf(A2);
                type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> memory* {
                    A0 a0 = m(args[0]);
                    A1 a1 = m(args[1]);
                    A2 a2 = m(args[2]);
                    member_init(A0, A1, A2)
                    member_call(a0, a1, a2)
                };
            }
            else if constexpr (member_fn<T>::n_args == 4) {
                using A0 = typename member_fn<T>::argument<0>::type;
                using A1 = typename member_fn<T>::argument<1>::type;
                using A2 = typename member_fn<T>::argument<2>::type;
                using A3 = typename member_fn<T>::argument<3>::type;
                type->method->args[0] = typeOf(A0);
                type->method->args[1] = typeOf(A1);
                type->method->args[2] = typeOf(A2);
                type->method->args[3] = typeOf(A3);
                type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> memory* {
                    A0 a0 = m(args[0]);
                    A1 a1 = m(args[1]);
                    A2 a2 = m(args[2]);
                    A3 a3 = m(args[3]);
                    member_init(A0, A1, A2, A3)
                    member_call(a0, a1, a2, a3)
                };
            }
            else {
                static_assert("implement more args");
            }
        } else if constexpr (!is_lambda2<T>()) {
            if constexpr (std::is_class<T>::value && std::is_default_constructible<T>::value)
                type->f.ctr      = [](void* a) -> void { new (a) T(); };
            if constexpr (has_ctr_str2<T>::value)
                type->f.ctr_str  = [](void* a, const str2 &v)  -> void { new (a) T(v); };
            if constexpr (!std::is_trivially_destructible<T>::value)
                type->f.dtr      = [](void* a) -> void { ((T*)a) -> ~T(); };
            if constexpr (has_ctr_mem <T>::value)
                type->f.ctr_mem  = [](void* a, A* mem) -> void { new (a) T(mem); };
            if constexpr (std::is_copy_constructible<T>::value)
                type->f.ctr_cp   = [](void* a, void* b) -> void { new (a) T(*(const T*)b); };
            if constexpr (has_bool<T>)
                type->f.boolean  = [](void* a) -> bool { return bool(*(T*)a); };

            /// mix is useful on trivial types, like vec2/3/4 rgba/8/32f
            if constexpr (has_mix<T>::value)
                type->f.mix      = [](void* a, void *b, void *c, double f) -> void {
                    *(T*)c = ((T*)a)->mix(*(T*)b, f);
                };
        
            if constexpr (has_etype<T>::value) {
                id*    etype = typeOf(typename T::etype);
                etype->ref = type; ///
                etype->traits |= traits::enum_primitive;
            }
            if constexpr (std::is_array<T>::value) {
                type->traits |= traits::primitive_array;
                type->ref = typeOf(typename std::remove_extent<T>::type);
            }
            if constexpr (ion::inherits<A, T>()) {
                if constexpr (has_parent<T>())
                    type->parent = typeOf(typename T::parent);
            }
            
        }

        register_type2(type, __PRETTY_FUNCTION__, sizeof(T), traits);

        if constexpr (!is_lambda2<T>()) {
            if constexpr (registered_instance_meta2<T>()) {
                static T *def = new T();
                type->meta    = new List(def->meta()); /// make a reference to this data
                for (iprop &p: (*type->meta)->elements<iprop>()) {
                    p.offset     = p.is_method ? 0 : (size_t(p.member_addr) - size_t(def));
                    p.parent_type = type; /// we need to store what type it comes from, as we dont always have this context

                    /// this use-case is needed for user interfaces without css defaults
                    if (!p.is_method) {
                        u8 *prop_def = &(((u8*)def)[p.offset]);
                        p.init_value = calloc64(1, p.type->base_sz);
                        if (p.type->f.ctr_cp)
                            p.type->f.ctr_cp(p.init_value, prop_def);
                    }
                }
                delete def;
                Hash     *pmap = new Hash(size_t(16));
                doubly   *meta = (doubly*)type->meta;
                for (iprop &prop: meta->elements<iprop>())
                    (*pmap)[*prop.key] = m::pointer(&prop);
                type->meta_map = pmap;
            }
        }

        return (id*)type;
    }
    
}

id::id(bool init) {
    static bool once;
    if (!once) {
        once = true;
        id::char_t = typeOf(char);
        id::i64_t  = typeOf(i64);
        id::u64_t  = typeOf(u64);
        id::types  = new ahashmap(64);
        push_type(id::char_t); /// when hashmap/doubly are invoked, their specialized for_type2 methods should return their info
        push_type(id::i64_t);
        push_type(id::u64_t);
    }
}

cstr parse_fn(const std::string &cn) {
    std::string      st = "with TT = ";
    std::string      en = ";";
    num		         p  = cn.find(st) + st.length();
    num              ln = cn.find(en, p) - p;
    std::string      nm = cn.substr(p, ln);
    auto             sp = nm.find(' ');
    std::string  s_name = (sp != std::string::npos) ? nm.substr(sp + 1) : nm;
    num ns = s_name.find("ion::");
    if (ns >= 0 && s_name.substr(0, ns) != "std")
        s_name = s_name.substr(ns + 5);
    cstr  name = util::copy(s_name.c_str());
    return name;
}

void describe_type(id *type, cstr name, sz_t sz, u64 traits) {
    type->name      = name;
    type->traits    = traits;
    type->base_sz   = sz;
    type->src       = type;
}

void push_type(id *type) {
    m key(type->name);
    m value(m::pointer(type));
    id::types->set(key, value);
}

void register_type2(id *type, const std::string &sig, sz_t sz, u64 traits) {
    std::string copy = sig;
    cstr name = parse_fn(sig);
    describe_type(type, name, sz, traits);
    push_type(type);
}

/// these are called before registration, by ident::init
id *primitive_type(symbol name, sz_t sz) {
    id *type = new id();
    memset(type, 0, sizeof(id));
    describe_type(type, (cstr)name, sz, traits::primitive);
    return type;
}

static m symbolize(cstr cs, id* type = typeOf(str2), i64 id = 0) {
    if (!type->symbols)
        type->symbols = new symbols2;

    m name(cs);
    type->symbols->by_name[name] = id;
    type->symbols->by_value[id]  = name;
    return name;
}

/// before i do enum, we need to simplify the hashing around types, their symbol lookup and their values (use newer hash list)
/// this is a Value<i32>

struct e:m {
    ///
    template <typename C>
    e(A *mem, C *inst) : m(mem) { }

    /// called in construction by enum class
    template <typename C>
    static typename C::etype convert(m raw, ion::symbol S, C *p) {
        type_t type = typeOf(C);
        e::initialize((C*)null, (typename C::etype)0, S, type);
        mx psym;
        if (raw.type() == typeOf(str2)) {
            char  *d = raw.get<char>();
            /// in json, the enum can sometimes be given in "23124" form; no enum begins with a numeric so we can handle this
            if (d[0] == '-' || isdigit(*d)) {
                std::string str = (ion::symbol)d;
                i64 num  = (i64)std::stoi(str);
                psym     = type->symbols->ids.lookup(num);
            } else {
                u64 hash = djb2(d);
                psym     = type->symbols->djb2.lookup(hash);
            }
            if (!psym) {
                /// if lookup fails, compare by the number of chars given
                for (m &mem: type->symbols->list.elements<m>()) {
                    if (raw.a_str->len != mem.a_str->len)
                        continue;
                    if (matches((ion::symbol)mem->origin, (ion::symbol)d, raw.mem->count))
                        return (typename C::etype)mem->id;
                }
            }
        } else if (raw.type() == typeOf(int)) {
            i64   id = i64(*raw.get<int>());
            psym     = type->symbols->ids.lookup(id);
        } else if (raw.type() == typeOf(i64)) {
            i64   id = *raw.get<i64>();
            psym     = type->symbols->ids.lookup(id);
        } else if (raw.type() == typeOf(typename C::etype)) {
            i64   id = *raw.get<typename C::etype>();
            psym     = type->symbols->ids.lookup(id);
        }
        if (!psym) {
            printf("symbol: %s, raw: %s\n", S, (char*)raw.mem->origin);
            fflush(stdout);
            throw C();
        }
        return (typename C::etype)(psym.mem->id);
    }

    template <typename C, typename E>
    E initialize(C *p, E v, ion::symbol names, type_t ty) {
        /// names should support normal enum syntax like abc = 2, abc2 = 4, abc3, abc4; we can infer what C++ does to its values
        /// get this value from raw.origin (symbol) instead of the S
        if (ty->secondary_init) return v;
        ty->secondary_init = true;
        str snames = str((cstr)names);
        array   sp = snames.split(", ");
        int      c = (int)sp.len();
        i64   next = 0;

        Array<str> split(sp); /// this is just a cast with different windowing
                                    /// it has the operator[] overloaded
                                    /// will probably do the same for Map where we use it that way
        for (int i = 0; i < c; i++) {
            num idx = split[i].index_of(str("="));
            if (idx >= 0) {
                str2 sym = split[i].mid(0, idx).trim();
                str2 val = split[i].mid(idx + 1).trim();
                symbolize(sym, ty, val.integer_value());
            } else {
                str2 sym = split[i].trim();
                symbolize(sym, ty, i64(next));
            }
            next = i + 1;
        };
        return v;
    }

    ///
    e() : m() { }
    ///
    template <typename E, typename C>
    e(E v, C *inst) : e(alloc<E>(&v), this) { }

    e(A *mem) : m(mem) {}

    bool matches(ion::symbol a, ion::symbol b, int len) {
        for (int i = 0; i < len; i++) {
            if (!a[i] || !b[i])
                return false;
            if (a[i] == b[i] || (a[i] == '-' && b[i] == '_') ||
                                (b[i] == '-' && a[i] == '_'))
                continue;
            return false;
        }
        return true;
    }

    ion::symbol symbol() {
        assert(v_i32->type == typeof(Value<i32>));
        i32 v = v_i32->value;
        assert(mem->type->symbols);
        m mem_symbol = mem->type->symbols->by_id(i64(v));
        if (!mem_symbol) printf("symbol: mem is null for value %d\n", (int)v);
        assert(mem_symbol);
        return (ion::symbol)mem_symbol;
    }
};

#define enums(C,D,...)\
    struct C:ex {\
        enum etype { __VA_ARGS__ };\
        enum etype&    value;\
        using parent_class  = ex;\
        using context_class = C;\
        using intern = etype;\
        inline static type_t intern_t;\
        static type_t  register_data()    { return typeOf(etype); }\
        static type_t  register_class()   { return typeOf(C); }\
        static memory* lookup(ion::symbol sym) { return typeOf(C)->lookup(sym); }\
        static memory* lookup(i64     id) { return typeOf(C)->lookup(id);  }\
        static doubly &symbols() { return typeOf(C)->symbols->list; }\
        inline static const int count = num_args(__VA_ARGS__);\
        inline static const str raw   = str_args(__VA_ARGS__);\
        str name() { return (char*)symbol(); }\
        struct memory *to_string() { return typeOf(C)->lookup(i64(value)); }\
        C(enum etype t = etype::D):ex(initialize(this,             t, (ion::symbol)raw.cs(), typeOf(C)), this), value(*get<enum etype>()) { }\
        C(size_t     t)           :ex(initialize(this, (enum etype)t, (ion::symbol)raw.cs(), typeOf(C)), this), value(*get<enum etype>()) { }\
        C(int        t)           :ex(initialize(this, (enum etype)t, (ion::symbol)raw.cs(), typeOf(C)), this), value(*get<enum etype>()) { }\
        C(str sraw):C(ex::convert(sraw, (ion::symbol)C::raw.cs(), (C*)null)) { }\
        C(mx  mraw):C(ex::convert(mraw, (ion::symbol)C::raw.cs(), (C*)null)) { }\
        C(ion::symbol sym):C(ex::convert(sym, (ion::symbol)C::raw.cs(), (C*)null)) { }\
        C(memory* mem):C(mx(mem)) { }\
        operator etype() const { return value; }\
        C &operator=(const C &b) {\
            if(mx::mem != b.mx::mem) {\
                ion::drop(mx::mem);\
                mx::mem = ion::hold(b.mx::mem);\
                value = b.value;\
            }\
            return *this;\
        }\
        bool    operator== (const enum etype &v) const { return value == v; }\
        bool    operator== (ion::symbol v) const {\
            if (!mem && !v)\
                return true;\
            memory *m = lookup(v);\
            return (int)m->id == (int)value;\
        }\
        bool    operator!= (const enum etype &v) const { return value != v; }\
        bool    operator>  (const C &b)          const { return value >  b.value; }\
        bool    operator<  (const C &b)          const { return value <  b.value; }\
        bool    operator>= (const C &b)          const { return value >= b.value; }\
        bool    operator<= (const C &b)          const { return value <= b.value; }\
        explicit operator int() const   { return int(value); }\
        explicit operator i64() const   { return i64(value); }\
        operator str()         { return symbol(); }\
    };

/// no Memory, memory, and mx should merge into m; can call it mx.
/// no notion of data/context
/// no intern class, no schema
/// implement meta in one place, on the data
/// transitionable, once we figure out how to handle 'mx' here
/// mx would be a user class of a, a class of its own
/// inside it needs to be able to link to any sort of data, not just primitive

/// keep mostly the same functionality on mx for an easier go

int main(int argc, char **argv) {
    AUser aclass;
    aclass->a = 10;
    aclass->b = 22;

    str2 s1 = "1";
    str2 s2 = s1;

    Lambda<int(const str2&)> test_fn = [](const str2& arg) -> int {
        //console.log("arg = {0}", { arg });
        return 1;
    };

    test_fn("1");

    str2 a = "test"; a += "1";

    List l { str2("test"), str2("2") };

    m mtest = l->pop();
    str2  v = mtest;
    Field f { "test1", "test2" };

    Hash hashmap {
        Field { "test1", "test2" }
    };

    m test1 = hashmap["test1"];
    str2 test1_str = test1;
    //str2 hm_result = hashmap["test"];

    map  def     {{ "source",  path(".") }, { "build", path(".") }, { "project", str("project") }};
    map  args    { map::parse(argc, argv, def) };
    str  project { args["project"] };
    path source  { args["source"]  };
    path build   { args["build"]   };
    path ta      { source / (project + ".ta") };

    if (!ta.exists()) {
        console.fault("tapestry project not found: {0}", { str(ta) });
        return -1;
    }
    console.log("project is {0}", { str(ta) });

    /// we should only have 1 mode, which will generate & build everytime; it will always check for differences in generate
    /// it could generate ninja too, but i think thats too easy to implement to rely on something like that

    /// the script will have access to our types, and their methods
    Array<Expression> e = Expression::parse(ta);
    Expression::exec(e);
    return e ? 0 : -1;
}