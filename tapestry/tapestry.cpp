#include <tapestry/tapestry.hpp>

/// use mx internally
#include <mx/mx.hpp>

namespace ion {

/*

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
        str         sp       = "$()![]/+-*:\"\'#"; /// needs string logic in here to make a token out of the entire "string inner part" without the quotes; those will be tokens neighboring
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

/*
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
    std::function<struct A*(void*, struct m*, int)> call; /// for methods
};

struct id {
    struct id       *src;     ///
    cstr             name;
    size_t           base_sz; /// a types base sz is without regards to pointer state
    size_t           traits;
    bool             pointer; /// allocate enough space for a pointer to be stored
    struct Hash     *meta; // prop_map
    method_info     *method;

    id() { }

    id(bool);

    static inline struct ahashmap *types;
    static inline id *char_t; /// cannot use hashmap/mx::hash without a registered char type
    static inline id *i64_t;
    static inline id *u64_t;

    struct f_table {
        std::function<void(void*)>         dtr;
        std::function<void*(void*)>        ctr;
        std::function<void*(void*, struct A*)>    ctr_mem;  /// for mx objects (assign from memory)
        std::function<void*(void*, struct string *)> ctr_str; /// for mx objects (copy convert)
        std::function<void*(void*,void*)>  ctr_cp;
        std::function<void(void*,void*,void*,double)> mix;
        std::function<bool(void*)>         boolean;
        std::function<u64(void*)>          hash;
        std::function<int(void*,void*)>    compare;
        std::function<struct m*(void*)>    m_ref;
        std::function<struct string*(void*)> to_str;
    } f;

    struct symbols2 *symbols;
    id              *intern; /// the A-type inside a user class
    id              *ref; /// if you are given a primitive enum, you can find the schema and symbols on ref (see: to_string)
    id              *parent; 
    bool             secondary_init; /// used by enums but flag can be used elsewhere.  could use 'flags' too

    void   *alloc();
    void   *ctr();
    void    dtr(void* alloc);
    void   *ctr_mem (A *mem);
    void   *ctr_str (struct string *v);
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

    iprop(const iprop &ref);

    template <typename M>
    M &member_ref(void *m) ;

    void *member_pointer(void *m) ;

    symbol name() const ;

    struct string* to_string();
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
    A(id *type = null) : type(type), refs(1), h(0) { }
    A   *hold() { refs++; return this; }
    void drop() { if (--refs <= 0) { delete this; } }
    virtual u64 hash() {
        console.fault("implement hash() for type {0}", { str(type->name) });
        return 0;
    }
    virtual int compare(const struct m &ref) const {
        console.fault("implement compare() for type {0}", { str(type->name) });
        return -1;
    }
    virtual struct string *to_string() {
        console.fault("implement to_string() for type {0}", { str(type->name) });
        return null;
    }
    virtual ~A() { }
};

/// intentionally different from string
struct asymbol:A {
    i32   id;
    int   len;
    char *name;
    u64 hash() {
        if (A::h) return A::h;
        A::h = 10000 + id;
        return (u64)id;
    }
    asymbol(const char *name, i32 id = 0) : A(typeOf(asymbol)) {
        len  = strlen(name);
        this->id   = id;
        this->name = new char[len + 1];
        memcpy(this->name, name, len + 1);
    }
};

template <typename T, typename = void> struct has_to_string : false_type { };
template <typename T>
struct has_to_string<T, std::enable_if_t<std::is_same_v<decltype(std::declval<T>().to_string()), string*>>> : true_type { };

template <typename T>
struct vector;

struct m;

using arr = vector<m>;

/// always working on string theory..
struct string:A {
    char *chars;
    int   length;
    int   alloc;

    string(int size = 64) : A(typeOf(string)) {
        chars = new char[size];
        alloc = size;
        length = 0;
        chars[0] = 0;
    }

    string(const char v) : A(typeOf(string)) {
        length = 1;
        alloc  = 2;
        chars  = new char[2];
        memcpy(chars, &v, length);
        chars[length] = 0;
    }

    string(const char *v, int v_len = -1) : A(typeOf(string)) {
        length = v_len == -1 ? strlen(v) : v_len;
        alloc = 64;
        while (alloc < length)
            alloc <<= 1;
        chars = new char[64];
        memcpy(chars, v, length);
        chars[length] = 0;
    }

    static string *input(int size) {
        string *str = new string(size);
        fgets(str->chars, size, stdin);
        return str;
    }

    string *format(arr *args);

    string(char *v, int v_len = -1) : string((const char *)v, v_len) { }

    arr *split(string *v) const;

    int index_of(string *s) const {
        char *f = strstr(chars, s->chars);
        s->drop();
        return f ? (int)(size_t)(f - chars) : -1;
    }

    string *mid(int start, int slen = -1) const {
        if (start < 0)
            start = start + length;
        int mid_len = slen >= 0 ? slen : (length - start + (slen + 1));
        assert(mid_len >= 0);
        return new string(&chars[start], mid_len);
    }

    string *trim() const {
        int offset = 0;
        while (chars[offset] == ' ') {
            offset++;
        }
        int slen = length - offset;
        for (;slen;slen--) {
            if (chars[offset + slen - 1] != ' ')
                break;
        }
        return new string(&chars[offset], slen);
    }

    char &operator[](int i) const {
        return chars[i];
    }

    void append(const char* v, int append_len = -1) {
        if (append_len == -1)
            append_len = strlen(v);
        if (alloc <= length - append_len) {
            alloc = math::max(128, alloc << 1);
            char *n = new char[alloc];
            memcpy(n, chars, length);
            memcpy(&n[length], &v, append_len);
            n[length += append_len] = 0;
            delete[] chars;
            chars = n;
        } else {
            memcpy(&chars[length], v, append_len);
            chars[length += append_len] = 0;
        }
    }

    void append(char v)                { append(&v, 1); }
    void append(string* v)             { append(v->chars, v->length); v->drop(); }
    void operator += (char          v) { append(&v, 1); }
    void operator += (const char*   v) { append(v, strlen(v)); }
    void operator += (string* v)       { append(v->chars, v->length); v->drop(); }

    explicit operator        cstr() const { return chars; }
    explicit operator ion::symbol() const { return ion::symbol(chars); }

    explicit operator i64() {
        char *last;
        return strtoll(chars, &last, 10);
    }

    explicit operator double() {
        char *last;
        return strtod(chars, &last);
    }

    int len() const { return length; }

    string* to_string() override {
        return (string*)hold();
    }

    u64 hash() {
        if (A::h) return A::h;
        return A::h = fnv1a_hash(chars, length);
    }

    int compare(const m &b) const override;

    explicit operator bool() const { return length > 0; }
};

template <typename> struct is_value : false_type { };

template <typename T>
struct Value:A {
    id *value_type;
    T value;

    Value(const T& v) : A(typeOf(Value)), value(v), value_type(typeOf(T)) { }

    int compare(const struct m &ref) const;
    
    u64 hash() {
        if (A::h) return A::h;
        id* type = typeOf(T);
        if (type->traits & traits::primitive)
            return (A::h = fnv1a_hash(&value, sizeof(T)));
        if constexpr (ion::inherits<A, T>()) /// is also in traits (or will be)
            return (A::h = value->hash());
        console.fault("implement hash for non-A-type");
        return 0;
    }

    string *to_string() {
        char buf[128];
        if constexpr (identical<T, bool>()) {
            strcpy(buf, value ? "true" : "false");
        } else if constexpr (is_integral<T>()) {
            snprintf(buf, sizeof(buf), "%lld", (i64)value);
        } else if constexpr (is_realistic<T>()) {
            snprintf(buf, sizeof(buf), "%.4f", value);
        } else if constexpr (has_to_string<T>()) {
            return value.to_string();
        } else {
            printf("non-A type %s needs to_string\n", value_type->name);
        }
        return new string((ion::symbol)buf);
    }

    operator bool() const { return bool(value); }
};

template <typename T> struct is_value<Value<T>> : true_type { };

/// these can be managed or unmanaged
struct Pointer:A {
    id   *type;
    void *ptr;
    bool  managed;
    Pointer(id* type, void *ptr, bool managed) : A(null), type(type), ptr(ptr), managed(managed) { }
    ~Pointer() {
        if (managed) free(ptr);
    }
    int compare(const m& b) const override;
    u64 hash() {
        if (type->f.hash)
            return A::h = type->f.hash(ptr);
        return A::h = (u64)ptr;
    }
    string *to_string() {
        if (type->f.to_str)
            return type->f.to_str(ptr);
        char buf[128];
        snprintf(buf, sizeof(buf), "%s/%p", type->name, ptr);
        return new string(buf);
    }
};

template <typename T>
struct Vector;

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
        asymbol     *a_symbol;
        A           *a;
    };
    m()           : a(null) { }
    m(null_t)     : m()     { }
    m(const u64  &i);
    m(const i64  &i);
    m(const u32  &i);
    m(const i32  &i);
    m(const u16  &i);
    m(const i16  &i);
    m(const u8   &i);
    m(const i8   &i);
    m(const char &i); /// we use const refs here so we have no ambiguity with the const T& ctr
    m(ion::symbol sym);
    m(cstr   str) : m((ion::symbol)str) { }
    m(A       *a) : a(a) { } /// this lets us pass around actual dynamically allocated A classes
    m(string  *a) : a(a) { } /// this lets us pass around actual dynamically allocated A classes
    m(const m& b) : a(b.a ? b.a->hold() : null) { }
    template <typename T>
    m(const T& any) {
        using TT = std::remove_pointer<T>::type;
        if constexpr (inherits<A, TT>()) {
            /// if its an A pointer, we can merely hold it (we never put A's on stack, this reference can be stack but it should not be)
            if constexpr (std::is_pointer<T>::value)
                a = ((TT*)any)->hold();
            else
                a = ((TT&)any).hold();
        } else if constexpr (has_intern<TT>()) {
            /// hold onto A class within user type
            a = any.data->hold();
        } else {
            /// this is so we can hold onto any data; it must be copied
            if constexpr (std::is_pointer<T>::value)
                a = new Pointer(typeOf(TT), new TT((TT&)*any), true);
            else
                a = new Pointer(typeOf(TT), new TT(any), true);
            a->type = typeOf(Pointer);
        }
    }


    static m from_string(cstr cs, id* type) {
        assert(type);
        
        if (type == typeOf(string)) {
            return new string(cs);
        } else if (type->traits & traits::integral) { /// u/i 8 -> 64
            if (type == typeOf(bool)) {
                std::string s = std::string(cs);
                std::transform(s.begin(), s.end(), s.begin(),
                    [](unsigned char c){ return std::tolower(c); });
                return new bool(s == "true" || s == "1" || s == "tru" || s == "yes");
            }
            else if (type == typeOf(u32))    return m(   u32(std::stoi(cs)));
            else if (type == typeOf(i32))    return m(   i32(std::stoi(cs)));
            else if (type == typeOf(u64))    return m(   u64(std::stoi(cs)));
            else if (type == typeOf(i64))    return m(   i64(std::stoi(cs)));
            else if (type == typeOf(u16))    return m(   u16(std::stoi(cs)));
            else if (type == typeOf(i16))    return m(   i16(std::stoi(cs)));
            else if (type == typeOf(u8))     return m(    u8(std::stoi(cs)));
            else if (type == typeOf(i8))     return m(    i8(std::stoi(cs)));
            console.fault("unknown numeric conversion");
            return m();
        /// float / double
        } else if (type->traits & traits::realistic) {
            if (type == typeOf(float))       return m( float(std::stod(cs)));
            else if (type == typeOf(double)) return m(double(std::stod(cs)));
            console.fault("unknown real type conversion");
            return m();
        } else {
            bool mx_based = (type->traits & traits::mx_obj);
            sz_t len = strlen(cs);
            string *value = new string(cs);
            void *alloc = type->f.ctr_str(null, value);
            if (mx_based) {
                value->drop();
                return m((A*)alloc);
            }
            value->drop();
            return m::pointer(type, alloc, true);
        }
    }

    static m pointer(id* type, void *ptr, bool managed = false) {
        m result;
        result.v_pointer       = new Pointer(type, (void*)ptr, managed);
        result.v_pointer->type = type;
        result.a->type         = typeOf(Pointer);
        return result;
    }

    template <typename T>
    static m pointer(T *ptr, bool managed = false) {
        return pointer(typeOf(T), (void*)ptr, managed);
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
    id *type() { return a ? a->type : typeOf(null_t); }

    bool operator==(const m &ref) const {
        return compare(ref) == 0;
    }

    int compare(const m &ref) const {
        if (a == ref.a) return 0;
        if (a->type != ref.a->type) return -1;
        return a->compare(ref);
    }
    
    operator bool() const { return (a && a->type->f.boolean) ? a->type->f.boolean(a) : false; }
    
    static m    get(const m& o, const m& prop);
    static void set(const m& o, const m& prop, const m& value);

    template <typename T>
    static m method(T& obj, const struct str2 &name, const Vector<m> &args);

    template <typename T>
    T* get() const {
        //static id* type_check = typeOf(T);
        //assert(type_check == a->type);
        return (T*)a;
    }

    template <typename T>
    operator T&() const {
        if (a->type == typeOf(Pointer)) { /// can handle non-A inheritance
            assert(v_pointer->type == typeOf(T)); /// inheritance check not supported here since it requires an alias protocol on the class
            return *(T*)v_pointer->ptr;
        }
        if (a->type->traits & traits::value) {
            Value<int> *v = v_i32;
            id* v_type = v_i32->value_type;
            void *ptr = &v->value;
            assert(v_type == typeOf(T));
            return *(T*)ptr;
        } else {
            /// inheritance check (requires A-class)
            bool inherits_a = inherits<A, T>();
            assert(inherits_a);

            id *type = a->type;
            id *T_type = typeOf(T);
            while (type) {
                if (type == T_type)
                    break;
                type = type->parent;
            }
            assert(type);
            return *(T*)a;
        }
    }

    ion::symbol symbol() const {
        if (a && a->type == typeOf(string))
            return a_str    ? ion::symbol(a_str->chars)   : ion::symbol(null);
        else if (a && a->type == typeOf(asymbol))
            return a_symbol ? ion::symbol(a_symbol->name) : ion::symbol(null);
        return null;
    }

    string *to_string() {
        return a->to_string();
    }
};

iprop::iprop(const iprop &ref) {
    key         = ref.key ? new m(*ref.key) : null;
    member_addr = ref.member_addr;
    offset      = ref.offset;
    type        = ref.type;
    parent_type = ref.parent_type;
    init_value  = ref.init_value;
    is_method   = ref.is_method;
}

string* iprop::to_string() {
    return (string*)(key ? key->a_str->hold() : null);
}

int Pointer::compare(const m& b) const {
    if (type->f.compare)
        return type->f.compare(ptr, b.v_pointer->ptr);
    return (size_t)ptr - (size_t)b.v_pointer->ptr;
}

iprop::iprop() : key(null), member_addr(0), offset(0), type(null), is_method(false) { }

template <typename M>
iprop::iprop(symbol name, const M &member) : key(new m(name)), member_addr((size_t)&member), type(typeOf(M)), is_method(false) { }

template <typename M, typename CL>
iprop::iprop(symbol name, const M &member, const CL* inst) : 
    key(new m(name)), member_addr((size_t)&member), type(signatureOf(CL, M, member_addr, sizeof(member))), is_method(true) { }

template <typename M>
M &iprop::member_ref(void *m) { return *(M *)handle_t(&cstr(m)[offset]); }

void *iprop::member_pointer(void *m) { return (void *)handle_t(&cstr(m)[offset]); }

symbol iprop::name() const { return key->symbol(); }

int string::compare(const m &arg) const {
    string *b = arg.get<string>();
    return strcmp(chars, b->chars);
}

template <typename T>
int Value<T>::compare(const struct m &ref) const {
    Value &b = *(Value*)ref.a;
    return value - b.value;
}

template <typename T> struct is_vector : false_type {};

struct ilist:A {
    protected:
    int count;
    public:
    ilist(id *type) : A(type) { count = 0; }
    virtual void remove(int index, int amount = 1) = 0;
    //virtual void push(const m &v) = 0; -- these abstract classes arent very useful; some of the list require m, some are T type; you cannot do that here.
    //virtual m pop() = 0;
    virtual void clear() = 0;
};

#include <tapestry/iterators.hpp>

template <typename T>
struct Vector;

template <typename T>
struct vector:ilist {
    friend Vector<T>;

    private:
    T  *elements;
    int alloc;
    int count;

    public:
    vector(int reserve = 32) : ilist(typeOf(vector)) {
        alloc = 0;
        count = 0;
        elements = null;
        resize(reserve);
    }

    vector(std::initializer_list<T> list) : vector() {
        for (auto i: list)
            push(i);
    }

    T &get(int index) const {
        return elements[index];
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
        
        new (&elements[count++]) T(v);
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
        return A::h = h;
    }

    int len() const { return count; }

    iterator<T> begin() const { return iterator<T>{elements, 0};     }
    iterator<T>   end() const { return iterator<T>{elements, (size_t)count}; }

    explicit operator T*() const {
        return elements;
    }

    explicit operator bool() const { return count > 0; }
};

template <typename T> struct is_vector<vector<T>> : true_type { };

arr *string::split(string *v) const {
    arr  *res    = new arr(length + 1);
    char *start  = chars;
    char *scan   = chars;
    char *sp     = v->chars;
    int   sp_len = v->len();
    assert(sp_len > 0);
    if (length > 0)
        for (;;) {
            if (*scan == 0 || strncmp(scan, sp, sp_len) == 0) {
                int str_len = (int)(size_t)(scan - start);
                if (str_len)
                    res->push(new string(start, str_len));
                scan += sp_len;
                start = scan;
                if (*scan == 0) break;
            } else
                scan++;
        }
    v->drop();
    return res;
}

string *string::format(arr *args) {
    string   *res = new string(length + 1 + args->len() * 128);
    char   *start = chars;
    char     *end = start + length;
    for (;*start;) {
        char *f = strstr(start, "{");
        if (f) {
            if (f[1] != '{') {
                res->append(start, (int)(size_t)(f - start));
                char *s = &f[1];
                char *number_start = s;
                for (; *s && *s != '}'; s++)
                    assert(isdigit(*s));
                assert(*s == '}');
                int   number_len = (int)(size_t)(s - number_start);
                char *number     = new char[number_len + 1];
                memcpy(number, number_start, number_len);
                number[number_len] = 0;
                int n = atoi(number);
                assert(n >= 0 && n < args->len());
                delete[] number;
                m &a  = args->get(n);
                m str = a.to_string();
                if (str.a_str) {
                    char *a_start = str.a_str->chars;
                    char *a_end   = str.a_str->chars + str.a_str->length;
                    res->append(a_start, (int)(size_t)(a_end - a_start));
                } else {
                    res->append("null", 4);
                }
                start = f + 1 + number_len + 1;
            } else {
                res->append(start, (int)(size_t)(f - start) + 1);
                start += 2;
            }
        } else {
            res->append(start, (int)(size_t)(end - start));
            break;
        }
    }
    args->drop();
    return res;
}

struct alist:ilist {
    aitem* first;
    aitem* last;
    alist() : ilist(null) {
        first = null;
        last  = null;
        count = 0;
    }
    //template <typename T>
    alist(std::initializer_list<m> list) : alist() {
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
    aitem *push(const m &v) {
        aitem *i = new aitem(v);
        i->h = ((m&)v).hash();
        if (last) {
            last->next = i;
            i->prev    = last;
            last       = i;
        } else
            first      = (last = i);
        count++;
        return i;
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

    template <typename T>
    list_iterable<T> elements() const { return list_iterable<T> { first, last }; }

    template <typename T>
    list_iterable_hash<T> elements(u64 hash) const { return list_iterable_hash<T>    { first, last, hash }; }

    list_iterable_items      items()         const { return list_iterable_items      { first, last }; }
    list_iterable_items_hash items(u64 hash) const { return list_iterable_items_hash { first, last, hash }; }
};

/// never use directly, like all A-classes
struct afield:A {
    m key;
    m value; /// these are more debuggable than simple A* pointers
    public:
    afield() : A(typeOf(afield)) { }
    afield(const m& k, const m& v) : afield() {
        key   = k;
        value = v;
    }
    u64 hash() {
        if (A::h) return A::h;
        return A::h = key.hash();
    }
    string *to_string() override;
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

#define A_decl(U, _A_) \
    private: \
    void init_type() { ((A*)data)->type = typeOf(_A_); } \
    public: \
    struct _A_* data; \
    using intern = _A_;\
    U(const m& ref); \
    U(m& ref); \
    U(A* data); \
    U(_A_* data); \
    U(const U &b); \
    U(U &b); \
    U(); \
    template <typename... Arg> \
    U(Arg&&... arg) : data(new _A_(std::forward<Arg>(arg)...)) { init_type(); } \
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
    operator _A_&() const; \
    U &operator=(const U &b); \
   ~U(); \

#define A_impl(U, _A_) \
    U::U(const m& ref)    : data((_A_*)(ref.a->hold())) { init_type(); } \
    U::U(m& ref)          : U((const m&)ref) { } \
    U::U(A* data)         : data((_A_*)data) { init_type(); } \
    U::U(_A_* data)       : data((_A_*)data) { init_type(); } \
    U::U(const U &b)      : data((_A_*)b.data->hold()) { } \
    U::U(U &b)            : U((const U&)b) { } \
    U::U()                : data(new _A_())  { data->type = typeOf(_A_); } \
    _A_* U::operator->   () const { return data; } \
         U::operator _A_*() const { return (_A_*)(data ? data->hold() : null); } \
         U::operator _A_&() const { return *data; } \
    U   &U::operator=(const U &b) { \
        if (data != b.data) { \
            data->drop(); \
            data = (_A_*)b.data->hold(); \
        } \
        return *this; \
    } \
    U::operator bool() const { return data ? bool(*data) : false; } \
    U::operator m() const { return m(data->hold()); } \
    U::~U() { data->drop(); } \


#define A_type(_A_) { ((A*)data)->type = typeOf(_A_); }
#define A_forward(_A_, ...)  data(new _A_(__VA_ARGS__)) A_type(_A_)

struct str2 {
    A_decl(str2, string)
    operator         cstr() const { return        cstr(*data); }
    ion::symbol    symbol() const { return ion::symbol(*data); }
    operator       double() const { return      double(*data); }
    operator          i64() const { return         i64(*data); }
    char &operator[](int i) const { return (*data)[i]; }
};
A_impl(str2, string) /// this is str


template<typename T, typename = void>
struct has_ctr_str2 : std::false_type {};
template<typename T>
struct has_ctr_str2<T, std::enable_if_t<std::is_constructible<T, const str2&>::value>> : std::true_type {};

template<typename T, typename Arg, typename = void>
struct has_constructor : std::false_type {};

template<typename T, typename Arg>
struct has_constructor<T, Arg, std::void_t<decltype(T(std::declval<Arg>()))>> : std::true_type {};


template <typename T, typename = std::void_t<>>
struct has_str2_op : std::false_type {};

template <typename T>
struct has_str2_op<T, std::void_t<decltype(static_cast<str2>(std::declval<T>()))>> : std::true_type {};

template <typename T>
constexpr bool has_str2 = has_str2_op<T>::value;

string *afield::to_string() {
    return str2("{0}, {1}");
}

struct Field {
    A_decl(Field, afield)
};
A_impl(Field, afield)

struct List {
    A_decl(List,  alist)

    List(std::initializer_list<m> list) : A_forward(alist, list)
};

A_impl(List, alist)

struct Item {
    A_decl(Item, aitem)
};
A_impl(Item, aitem)


template <typename T, typename = void> struct registered_instance_meta2 : false_type { };
template <typename T>
struct registered_instance_meta2<T, std::enable_if_t<std::is_same_v<decltype(std::declval<T>().meta()), List>>> : true_type { };


template <typename T>
struct Vector {
    private:
    struct vector<T>* data;
    public:
    using intern = vector<T>;

    Vector()                : data(new vector<T>())             A_type(vector<T>)
    Vector(vector<T>* data) : data((vector<T>*)data)            A_type(vector<T>)
    Vector(const m& ref)    : data((vector<T>*)(ref.a->hold())) A_type(vector<T>)
    Vector(m& ref)          : Vector((const m&)ref)             { }
    Vector(const Vector &b) : data((vector<T>*)b.data->hold())  { }
    Vector(Vector &b)       : Vector((Vector &)b)               { }
    Vector(int size)        : data(new vector<T>(size))         A_type(vector<T>)

    Vector(std::initializer_list<T> args) : Vector(args.size()) {
        for (auto a: args)
            data->push(a);
    }

    //template <typename... Arg>
    //Vector(Arg&&... arg) : data(new vector<T>(std::forward<Arg>(arg)...)) A_type(vector<T>)

    Vector& operator += (const T& v) { (*data) += v;     return *this; }
    Vector& operator -= (int  index) { (*data) -= index; return *this; }

    iterator<T> begin() const { return data->begin(); }
    iterator<T>   end() const { return data->end(); }

    operator T*() const {
        return data->elements;
    }

    operator bool() const { return data->len() > 0; }
    operator m() const { return m((A*)data->hold()); }
    
    vector<T>* operator->() const { return data; }
    operator vector<T>*() const { return (vector<T>*)data->hold(); }
    Vector &operator=(const Vector &b) {
        if (data != b.data) {
            data->drop();
            data = b.data->hold();
        }
        return *this;
    }

    ~Vector() {
        data->drop();
    }

    T &operator[](int index) const {
        assert(index >= 0 && index < data->len());
        return data->elements[index];
    }
};

/// type is not set on these primitive structs; we dont construct them ourselves, and the macro generally does this
/// the exception is here, in the m generics. for all uses of A-type, we set their type
m::m(const u64    &i) : v_u64(new Value<u64>(i))    { a->type = typeOf(Value<u64>); }
m::m(const i64    &i) : v_i64(new Value<i64>(i))    { a->type = typeOf(Value<i64>); }
m::m(const u32    &i) : v_u32(new Value<u32>(i))    { a->type = typeOf(Value<u32>); }
m::m(const i32    &i) : v_i32(new Value<i32>(i))    { a->type = typeOf(Value<i32>); }
m::m(const u16    &i) : v_u16(new Value<u16>(i))    { a->type = typeOf(Value<u16>); }
m::m(const i16    &i) : v_i16(new Value<i16>(i))    { a->type = typeOf(Value<i16>); }
m::m(const u8     &i) : v_u8 (new Value<u8 >(i))    { a->type = typeOf(Value<u8>); }
m::m(const i8     &i) : v_i8 (new Value<i8 >(i))    { a->type = typeOf(Value<i8>); }
m::m(const char   &i) : v_u8 (new Value<u8>((u8)i)) { a->type = typeOf(Value<u8>); }
m::m(ion::symbol sym) : a_str(new string(sym)) { a->type = typeOf(string);    }

struct ahashmap:A {
    private:
    alist *h_fields;
    alist *list;
    int    count;
    int    sz;
    public:

    int len() const {
        return count;
    }

    static u64 hash_value(m &key) {
        return key.hash();
    }

    static u64 hash_index(m &key, int mod) {
        return hash_value(key) % mod;
    }

    alist &list_for_key(u64 k) {
        assert(sz > 0 && k < sz);
        return h_fields[k];
    }

    void push(const Field &f) {
        u64 k = hash_index(f->key, sz); // if sz is 1 for non-hash use-cases, that would be a reduction
        alist &b = list_for_key(k);
        f->h = f->hash();
        aitem *i = b.push(f);
        i->peer = list->push(f);
        i->peer->hold();
        count++;
    }
    
    ~ahashmap() {
        delete h_fields;
        delete list;
    }

    ahashmap(int sz = 32);
    //ahashmap(std::initializer_list<afield> a) : ahashmap(a.size() > 0 ? a.size() : 32) {
    //    for (auto &v: a)
    //        push(v);
    //}

    template<typename... Fields>
    ahashmap(const Field& first, Fields&&... fields) : ahashmap(32) {
        push(first);
        // expand pack and push each field
        (push(fields), ...);
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

    operator bool() { return *list; }

    template <typename T>
    list_iterable<T> elements() const { return list->elements<T>(); }

    template <typename T>
    list_iterable_hash<T> elements(u64 hash) const { return list->elements<T>(hash); }

    list_iterable_items      items()         const { return list->items(); }
    list_iterable_items_hash items(u64 hash) const { return list->items(hash); }
};

aitem* ahashmap::item(const m &key, alist **h_list, u64 *phash) {
    const u64 hash = hash_value((m&)key);
    const u64 k    = hash % sz;
    if (phash) *phash = hash;
    alist &hist = list_for_key(k);
    if (h_list) *h_list = (alist*)&hist;
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
    alist  *h_list = null;
    u64     hash = 0;
    aitem  *fi   = item(key, &h_list, &hash);
    afield *f    = null;
    if (!fi) {
        f = new afield(key, null);
        aitem* i = h_list->push(f); /// we iterate with a filter on hash id in doubly
        i->peer = list->push(f);
        count++;
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
    alist *h_list = null;
    aitem *fi     = item(key, &h_list);
    if (fi && h_list) {
        list->remove(fi->peer); /// weak ref, no drop needed
        h_list->remove(fi);
        count--;
        return true;
    }
    return false;
}

bool ahashmap::contains(const m &key) {
    return ahashmap::item(key, null);
}

ahashmap::ahashmap(int sz) : A() {
    h_fields = new alist[sz];
    list     = new alist;
    count    = 0;
    this->sz = sz;
}


struct Hash {
    A_decl(Hash, ahashmap)

    template<typename... Fields>
    Hash(const Field& first, Fields&&... fields) : data(new ahashmap(first, std::forward<Fields>(fields)...)) {
        data->type = typeOf(ahashmap);
    }

    void set(const m& key, const m& val) {
        data->set(key, val);
    }

    m &operator[](const m &key) { return (*data)[key]; }

    m &operator[](i64 key) { return (*data)[key]; }
    m &operator[](i32 key) { return (*data)[key]; }
};
A_impl(Hash, ahashmap)


m m::get(const m& o, const m& prop) {
    id   *type = o.a->type == typeOf(Pointer) ? o.v_pointer->type     : o.a->type;
    u8   *ptr  = o.a->type == typeOf(Pointer) ? (u8*)o.v_pointer->ptr : (u8*)o.a;
    assert(type->meta);
    Hash &meta = *type->meta;
    assert(meta->len() > 0);
    afield *f = meta->fetch(prop);
    assert(f);
    iprop &pr = f->value; /// performs a type-check
    u8 *member_ptr = &ptr[pr.offset];
    assert(pr.type->f.m_ref);
    m* ref = pr.type->f.m_ref(member_ptr);
    m cp = *ref;
    delete ref;
    return cp;
}

void m::set(const m& o, const m& prop, const m& value) {
    m ref = m::get(o, prop);
    assert(ref.a->type == typeOf(Pointer));
    /// check if value is a Value<T>
    /// that is for user types, primitives, misc structs (non-A)
    if (value.a->type->traits & traits::value) {
        /// values are copy constructed
        id*  value_type =  value.v_i32->value_type;
        void* value_ptr = &value.v_i32->value; /// will be the same for any value
        assert(value_type == ref.v_pointer->type);
        if (ref.v_pointer->type->f.dtr)
            ref.v_pointer->type->f.dtr(ref.v_pointer->ptr);
        ref.v_pointer->type->f.ctr_cp(ref.v_pointer->ptr, value_ptr);
    } else {

    }
}

struct logger2 {
    inline static Lambda<void(m)> service;

    protected:
    static void _print(const str2 &st, const Vector<m> &ar) {
        static std::mutex mtx;
        mtx.lock();
        str2 msg = st->format(ar);
        if (service) service(msg);
        fputs(msg->chars, stdout);
        fputs("\n", stdout);
        fflush(stdout);
        mtx.unlock();
    }

    public:
    inline void log(const m& msg, const Vector<m> &ar = {}) {
        _print(((m&)msg).to_string(), ar);
    }

    void test(const bool cond, const m& str = {}, const Vector<m> &ar = {}) {
        #ifndef NDEBUG
        if (!cond) {
            _print(((m&)str).to_string(), ar);
            exit(1);
        }
        #endif
    }

    static str2 input() {
        return string::input(1024);
    }

    inline void fault(const m& msg, const Vector<m>& ar = { }) { _print(((m&)msg).to_string(), ar); brexit(); }
    inline void error(const m& msg, const Vector<m>& ar = { }) { _print(((m&)msg).to_string(), ar); brexit(); }
    inline void print(const m& msg, const Vector<m>& ar = { }) { _print(((m&)msg).to_string(), ar); }
    inline void debug(const m& msg, const Vector<m>& ar = { }) { _print(((m&)msg).to_string(), ar); }
};

logger2 console2;

extern logger2 console2;

struct symbols2 {
    Hash   by_name  { };
    Hash   by_value { };
    List   list;
};

void id::init() {
    static id* id_types;
    if (!id_types) id_types = new id(true);
}

// Define a helper type for SFINAE
template<typename T, bool hasIntern>
struct intern_of_ {
    using type = T;
};

template<typename T>
struct intern_of_<T, true> {
    using type = typename T::intern;
};

template<typename T>
using intern_of = typename intern_of_<T, has_intern<T>::value>::type;

template <typename TT, typename CL>
id *id::for_type(void *MPTR, size_t MPTR_SZ) {
    id::init();
    using T = pure_type<TT>;

    static id* type; if (type) return type; /// assert the name and traits are set

    /// static identity: make sure we only proceed to pure type's definition; otherwise we are redefining id*    multiple times
    /// side effect is one more call
    if constexpr (!identical<T, TT>()) return for_type<T>(); 

    type       = new id();
    memset(type, 0, sizeof(id));
    bool is_A  = ion::inherits<A, T>();
    u64 traits = (is_primitive<T> ()  ? traits::primitive : 0) |
                 (is_integral <T> ()  ? traits::integral  : 0) |
                 (is_realistic<T> ()  ? traits::realistic : 0) | // if references radioshack catalog
                 (is_array    <T> ()  ? traits::array     : 0) |
                 (is_lambda2  <T> ()  ? traits::lambda    : 0) |
                 (is_map      <T> ()  ? traits::map       : 0) |
                 (is_value    <T> ()  ? traits::value     : 0) |
                 (has_etype<T>::value ? traits::mx_enum   : 0) |
                 (is_A                ? traits::mx_obj    : 0);

    if constexpr (has_intern<T>())
        type->intern = id::for_type<typename T::intern>(MPTR, MPTR_SZ);
    
    if constexpr (std::is_member_function_pointer<T>::value) {
        using R = typename member_fn<T>::r_type;
        type->method = new method_info {
            .args      = (id**)calloc(member_fn<T>::n_args, sizeof(id*)),
            .r_type    = typeOf(R),
            .arg_count = member_fn<T>::n_args
        };

        #undef member_init
        #define member_init(...) \
            CL *obj = (CL*)inst; \
            R (CL::*member_func)(__VA_ARGS__); \
            assert(sizeof(member_func) == MPTR_SZ); \
            memcpy((void*)&member_func, (void*)MPTR_MEM, MPTR_SZ); \
            m result;

        #undef member_call
        #define member_call(...) \
            if constexpr (identical<R, void>()) \
                (obj->*member_func)(__VA_ARGS__); \
            else \
                result = (obj->*member_func)(__VA_ARGS__); \
            return result.a ? result.a->hold() : (A*)null;
        
        u8 MPTR_MEM[1024];
        memcpy(MPTR_MEM, MPTR, MPTR_SZ);
        if constexpr (member_fn<T>::n_args == 0) {
            type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> A* {
                member_init()
                member_call()
            };
        }
        else if constexpr (member_fn<T>::n_args == 1) {
            using A0 = typename member_fn<T>::argument<0>::type;
            type->method->args[0] = typeOf(A0);
            type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> A* {
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
            type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> A* {
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
            type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> A* {
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
            type->method->call = [MPTR_MEM, MPTR_SZ](void* inst, m * args, int n_args) -> A* {
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
    } else if constexpr (!is_lambda2<T>() && !inherits<A, T>()) {
        using K = intern_of<T>;

        if constexpr (std::is_default_constructible<T>::value)
            type->f.ctr      = [](void *a) -> void* { if (a) new (a) T(); else a = (void*)new T(); return a; };
        
        if constexpr (has_ctr_str2<K>::value)
            type->f.ctr_str  = [](void* a, string *v) -> void* { str2 sv((A*)v); if (a) new (a) T(sv); else a = (void*)new T(sv); return a; };
        
        if constexpr (!std::is_trivially_destructible<T>::value)
            type->f.dtr      = [](void* a) -> void { ((T*)a) -> ~T(); };
        
        if constexpr (has_constructor<T, A*>::value)
            type->f.ctr_mem  = [](void *a, A* mem) -> void* { if (a) new (a) T(mem); else a = (void*)new T(mem); return a; };
        
        if constexpr (std::is_copy_constructible<T>::value)
            type->f.ctr_cp   = [](void* a, void* b) -> void* { if (a) new (a) T(*(const T*)b); else a = (void*)new T(*(const T*)b); return a; };

        if constexpr (has_bool<T>)
            type->f.boolean  = [](void* a) -> bool { return bool(*(T*)a); };

        if constexpr (has_to_string<T>())
            type->f.to_str   = [](void* a) -> string* { return ((T*)a)->to_string(); };

        if constexpr (has_hash<T>())
            type->f.hash     = [](void* a) -> u64 { return ((T*)a)->hash(); };

        if constexpr (has_compare<T>())
            type->f.compare  = [](void* a, void* b) -> int { return ((T*)a)->compare(*(T*)b); };

        if constexpr (is_convertible<T, m>())
            type->f.m_ref    = [](void* a) -> m* { return new m(m::pointer((T*)a, false)); };

        /// mix is useful on trivial types, like vec2/3/4 rgba/8/32f
        if constexpr (has_mix<T>::value)
            type->f.mix      = [](void* a, void *b, void *c, double f) -> void {
                *(T*)c = ((T*)a)->mix(*(T*)b, f);
            };
    
        if constexpr (has_etype<T>::value) {
            id*    etype = typeOf(typename T::etype);
            etype->ref = type; ///
            type->ref = etype;
            etype->traits |= traits::enum_primitive;
        }
    }

    if constexpr (ion::inherits<A, T>()) {
        if constexpr (has_parent<T>())
            type->parent = typeOf(typename T::parent);
    }

    register_type2(type, __PRETTY_FUNCTION__, sizeof(T), traits);

    if constexpr (!is_lambda2<T>()) {
        if constexpr ((inherits<A, T>() || !has_intern<T>()) && registered_instance_meta2<T>()) {
            static T *def = new T();
            List mlist = def->meta();
            type->meta = new Hash(16);
            for (m &element: mlist->elements<m>()) {
                iprop &pr = element;
                pr.offset = pr.is_method ? 0 : (size_t(pr.member_addr) - size_t(def));
                pr.parent_type = type;
                /// obtain default value for this member
                if (!pr.is_method) {
                    u8 *prop_def = &(((u8*)def)[pr.offset]);
                    if (pr.type->f.ctr_cp)
                        pr.init_value = pr.type->f.ctr_cp(null, prop_def);
                }
                (*type->meta)->set(*pr.key, element);
            }
            delete def;
        }
    }

    return (id*)type;
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

static m symbolize(cstr cs, id* type = typeOf(string), i32 id = 0) {
    if (!type->symbols)
        type->symbols = new symbols2;

    m name(cs);
    type->symbols->by_name[name] = id;
    type->symbols->by_value[id]  = name;
    type->symbols->list += new asymbol(name.symbol(), id);
    return name;
}

struct e:Value<i32> {
    static int convert(m raw, ion::symbol S, id* type) {
        initialize(0, S, type);
        m sym;
        if (raw.type() == typeOf(asymbol))
            return raw.a_symbol->id;
        if (raw.type() == typeOf(string)) {
            str2 raw_str = raw;
            /// in json, the enum can sometimes be given in "23124" form; no enum begins with a numeric so we can handle this
            if (raw_str[0] == '-' || isdigit(raw_str[0])) {
                i32 id   = (i32)i64(raw_str);
                sym      = type->symbols->by_value[id];
            } else {
                sym      = type->symbols->by_name[raw];
            }
            if (!sym) {
                /// if lookup fails, compare by the number of chars given
                for (asymbol &f: type->symbols->list->elements<asymbol>()) { /// changing this from str to a field (so we may have the id on symbols)
                    if (raw_str->len() != f.len)
                        continue;
                    if (matches((ion::symbol)f.name, (ion::symbol)raw_str, raw_str->len()))
                        return f.id;
                }
            }
        } else if (raw.type() == typeOf(i32) || raw.type() == type->ref) { // make sure ref is set
            i32   id = i32(raw);
            sym      = type->symbols->by_value[id];
        } else if (raw.type() == typeOf(i64)) {
            i32   id = i32(i64(raw));
            sym      = type->symbols->by_value[id];
        }
        if (!sym) {
            printf("symbol: %s, raw: %s\n", S, (ion::symbol)raw);
            fflush(stdout);
            return 0;
        }
        return sym.a_symbol->id;
    }

    static int initialize(int v, ion::symbol names, id* type) {
        if (type->secondary_init) return v;
        type->secondary_init = true;
        str2 snames = str2(names);
        arr     *sp = snames->split(new string(", "));
        int       c = sp->len();
        i32    next = 0;
        string   *e = new string("=");
        int       i = 0;
        for (string &s: *sp) {
            num idx = s.index_of((string*)e->hold());
            if (idx >= 0) {
                str2 sym = s.mid(0, idx);
                sym = sym->trim();
                str2 val = s.mid(idx + 1);
                val = val->trim();
                symbolize(sym, type, i32(i64(val)));
            } else {
                str2 sym = s.trim();
                symbolize(sym, type, i32(next));
            }
            next = i + 1;
        };
        e->drop();
        return v;
    }

    e(int value) : Value<i32>(value) {
    }

    static bool matches(ion::symbol a, ion::symbol b, int len) {
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
        assert(type == typeOf(Value<i32>));
        i32 v = value;
        assert(type->symbols);
        m mem_symbol = type->symbols->by_value[v];
        if (!mem_symbol) printf("symbol: mem is null for value %d\n", (int)v);
        assert(mem_symbol);
        return (ion::symbol)mem_symbol;
    }

    string *to_string() override { return type->symbols->by_value[i32(value)].a_str; }
};

constexpr int num_occurances2(const char* cs, char c) {
    return cs[0] ? (cs[0] == c) + num_occurances2(cs + 1, c) : 0; 
}

#define num_args2(...) (num_occurances2(#__VA_ARGS__, ',') + 1)
#define str_args2(...) (str2(#__VA_ARGS__))


template <typename T>
m m::method(T& obj, const str2 &name, const Vector<m> &args) {
    id* type = typeOf(T);
    if (type->intern)
        type = type->intern;
    afield *f = (*type->meta)->fetch(name);
    assert(f);
    iprop &pr = f->value;
    method_info *method = pr.type->method;

    m result;
    if (method) {
        int n_args = args->len();
        assert(n_args == method->arg_count);

        m *mem_args = (m*)calloc(n_args, sizeof(m));
        for (int a = 0; a < n_args; a++) {
            m  &src = args[a];
            m  &dst = mem_args[a];
            id *atype = method->args[a];
            if (src.a->type != atype) {
                m   msrc(src.a->hold());
                string *st = msrc.to_string();
                m   conv = m::from_string(st->chars, atype);
                st->drop();
                dst = m(conv.a->hold());
            } else
                dst = src;
        }
        result = method->call((void*)&obj, mem_args, n_args);
        for (int a = 0; a < n_args; a++)
            mem_args[a].a->drop();
        free(mem_args);
    }
    return result;
}


#define enums2(C,D,...) \
    struct C { \
        private: \
        e* data; \
        void init_type() { \
            data->type = typeOf(C); \
        } \
        public: \
        enum etype { __VA_ARGS__ }; \
        enum etype    value; \
        using intern = e; \
        static int         lookup_value(ion::symbol sym) { return typeOf(C)->symbols->by_name [sym];   } \
        static ion::symbol lookup_name (i32       value) { return typeOf(C)->symbols->by_value[m(value)]; } \
        static List &symbols() { return typeOf(C)->symbols->list; } \
        inline static const int count  = num_args2(__VA_ARGS__); \
        inline static const str2 raw   = str_args2(__VA_ARGS__); \
        str2 name() { return (char*)symbol(); } \
        ion::symbol symbol() { return data->type->symbols->by_value[value]; } \
        C(enum etype t = etype::D):data(new e(e::initialize(t, (ion::symbol)raw, typeOf(C)))), value(t) { init_type(); } \
        C(int t)                  :C((enum etype)t) { } \
        C(const str2 &sraw)       :C(e::convert(sraw, (ion::symbol)raw, typeOf(C))) { } \
        C(const m &mraw)          :C(e::convert(mraw, (ion::symbol)raw, typeOf(C))) { } \
        C(ion::symbol sym)        :C(e::convert(sym,  (ion::symbol)raw, typeOf(C))) { } \
        C(e* mem)                 :data((e*)mem->hold()) { } \
        C(A* mem)                 :data((e*)mem->hold()) { } \
        operator   etype() const { return value; } \
        C &operator=(const C &b) { \
            if(data != b.data) { \
                if (data) data->drop(); \
                data = (e*)b.data->hold(); \
                value = b.value; \
            } \
            return *this;\
        } \
        bool    operator== (const enum etype &v) const { return value == v; }\
        bool    operator== (ion::symbol sym) const { \
            if (!data && !sym) \
                return true; \
            i32 id = data->type->symbols->by_name[sym]; \
            return id == (i32)value; \
        } \
        bool    operator!= (const enum etype &v) const { return value != v; } \
        bool    operator>  (const C &b)          const { return value >  b.value; } \
        bool    operator<  (const C &b)          const { return value <  b.value; } \
        bool    operator>= (const C &b)          const { return value >= b.value; } \
        bool    operator<= (const C &b)          const { return value <= b.value; } \
        explicit operator int() const   { return value; } \
        explicit operator i64() const   { return i64(value); } \
        operator str2()                 { return symbol(); } \
    };

/// no Memory, memory, and mx should merge into m; can call it mx.
/// no schema
/// implement meta in one place, on the data or trivial types
/// transitionable, once we figure out how to handle 'mx' here
/// mx would be a user class of a, a class of its own
/// inside it needs to be able to link to any sort of data, not just primitive
/// keep mostly the same functionality on mx for an easier go
*/


enums(EnumTest, undefined,
    undefined, one, two, three, four)

struct atest:A {
    int test1;
    int test2;

    atest() : A(typeof(atest)) { }

    int method1(int arg1, int arg2) {
        return arg1 + arg2;
    }

    List meta() {
        return {
            prop { "test1", test1 },
            prop { "method1", &atest::method1, this }
        };
    }

    explicit operator bool() {
        return test1 > 0;
    }
};

struct ATest {
    A_decl(ATest, atest)
};

A_impl(ATest, atest);


int main(int argc, char **argv) {
    prop p1 = prop();
    p1.key = new M("test1");
    M p1_contained(p1);

    console.log("test {1} {0} {2}", { "test1", 2, 1, iprop{} });

    ATest a2;
    id* type = typeof(atest);
    Hash *meta = type->meta;
    M mtest1 = "test1";
    field *f = (*meta)->fetch(mtest1);

    int a2_test11 = M::get(a2, "test1");
    id* int_type = typeof(int);

    M::set(a2, "test1", int(2));

    M res = M::method(a2, "method1", { 22, 22 });

    console.log("result = {0}", { res });

    console.log("a2.test1 = {0}", { a2->test1 });
    int a2_test12 = M::get(a2, "test1");

    str      s1 = "1";
    str      s2 = s1;
    EnumTest e1;
    EnumTest e2 = "one";

    Vector<EnumTest> enums { e1, e2 };
    EnumTest &ie2 = enums[1];

    for (EnumTest &e: enums)
        printf("enum value = %d\n", (int)e.value);

    /*
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
    */
    return 0;
}