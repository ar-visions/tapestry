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

struct Field:mx {
    struct M {
        Array<str> strings;
        int compare(Field::M &b) {
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
        u64 hash() {
            u64 h = OFFSET_BASIS;
            for (str &fs: strings) {
                h *= FNV_PRIME;
                h ^= fs.hash();
            }
            return h;
        }
    };
    Field(str s) : Field() {
        data->strings = Array<str> { s };
    }
    Field(Array<str> strings) : Field() {
        data->strings = strings;
    }
    bool operator==(const Field &s) const {
        return data->strings == s->strings;
    }
    bool operator!=(const Field &s) const {
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
    mx_basic(Field);
};

static Array<str> keywords = { "method", "import" };

struct Line {
    Array<Field> tokens;
    int indent;
    int num;
    
    int find(str token) {
        int i = 0;
        for (Field &t: tokens) {
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

    static Field parse_token(char *start, sz_t len) {
        while (start[len - 1] == '\t' || start[len - 1] == ' ')
            len--;
        str all { start, len };
        char t = all[0];
        return (t == '-' || (t >= '0' && t <= '9')) ? Field(all) : Field(all.split(".")); /// mr b: im a token!
    }

    static Array<Line> read_tokens(str input) {
        str         sp       = "$()![]+-*/:\"\'#"; /// needs string logic in here to make a token out of the entire "string inner part" without the quotes; those will be tokens neighboring
        char        until    = 0; /// either ) for $(script) ", ', f or i
        sz_t        len      = input.count();
        Array<Line> res;
        Array<Field> tokens;
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

    static memory *lookup(const Array<map> &stack, Field ident, bool top_only) {
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
                assert(op->value.type() == typeof(Field));
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
    Array<Field>  tokens;
    int           count;
    int           index;
    
    Descent(Array<Field> &a_tokens, int from, int count) : tokens(sz_t(count)), index(0), count(count) {
        int ii = 0;
        for (int i = from; i < from + count; i++)
            tokens += a_tokens[i];
        tokens += Field {};
    }
    
    bool expect(const Field &token) {
        assert(index < count);
        if (tokens[index] != token)
            console.fault("expected token: {0}", {token});
        return true;
    }

    Op::Type is_numeric() {
        Field &token = tokens[index];
        if (token.len() > 1)
            return Op::Type::Undefined;
        char t = tokens[index][0][0];
        return (t >= '0' && t <= '9') ? (strchr(token[0].data, '.') ? Op::Type::LiteralReal : Op::Type::LiteralInt) : Op::Type::Undefined;
    }

    Op::Type is_var() { /// type, var, or method (all the same); its a name that isnt a keyword
        Field &token = tokens[index];
        char t = token[0][0];
        if (isalpha(t) && keywords.index_of(token[0]) == -1) {
            /// lookup against variable table; declare if in isolation
            return Op::Type::Var;
        }
        return Op::Type::Undefined;
    }

    Field consume(const Field &token) {
        expect(token);
        return tokens[index++];
    }

    Field consume() {
        assert(index < count);
        return tokens[index++];
    }

    Field &next() {
        Field &nx = tokens[index];
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
            Field &f = next();
            assert(f.len() == 1);
            cstr cs = f[0].data;
            bool is_int = n == Op::Type::LiteralInt;
            consume();
            return Op::value(n, mx::from_string(cs, is_int ? typeof(i64) : typeof(double)));
        }
        Op::Type i = is_var(); /// its a variable or method (same thing; methods consume optional args)
        if (i) {
            Field &f = next();
            consume();
            return Op::value(i, f); /// the entire Field is value
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
    
    static Op parse_ops(Array<Field> &tokens, int start, int count) {
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
                    Field f = line.tokens[0];
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
            if (key.type() == typeof(Field)) { /// set variable to value
                top[key] = val; /// val is null and should be i64:1
                if (e->children)
                    exec_expressions(e->children, stack);
            } else if (key.type() == typeof(Array<mx>)) { /// elements loop
                assert(val.type() == typeof(Field));
                if (e->children) {
                    Array<mx> a(hold(key.mem));
                    for (mx &element: a) {
                        top[val] = element; /// each iteration, assign the element to this Field
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

/// we cannot actually move these around so easy without a copy
/// all ax-classes would need to be contained in pointer containment
/// we would never actually create anything 'ax' or sub derived without a new keyword, must be stack
/// if by hack we can enforce this


/// we cannot ever put an 'A' class on stack, as we do with mx currently
/// that could mean, however, mx could be a user of A
struct A { /// runtime of silver-lang
    type_t  type;
    int     refs; /// no schema, since we'll use traditional c++ 17
    A() : type(null), refs(0) { }
    A   *hold() { refs++; return this; }
    void drop() { if (--refs <= 0) delete this; }
};

struct a_user:A {
    int a;
    int b;
};

template <typename T>
struct Value:A {
    T value;
    Value(const T& v) : value(v) { }
};

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
        string      *a_str;
        A           *a;
    };
    m() : a(null) { }
    m(u64 i) : v_u64(new Value<u64>(i)) { }
    m(i64 i) : v_i64(new Value<i64>(i)) { }
    m(u32 i) : v_u32(new Value<u32>(i)) { }
    m(i32 i) : v_i32(new Value<i32>(i)) { }
    m(u16 i) : v_u16(new Value<u16>(i)) { }
    m(i16 i) : v_i16(new Value<i16>(i)) { }
    m(u8  i) : v_u8 (new Value<u8 >(i)) { }
    m(i8  i) : v_i8 (new Value<i8 >(i)) { }
    m(A  *a) : a(a ? a->hold() : null) { } /// this lets us pass around actual dynamically allocated A classes
    m(const m& b) : a(b.a ? b.a->hold() : null) { }
    m& operator=(m &b) {
        if (a != b.a) {
            if (a) a->drop();
            a = b.a->hold();
        }
        return *this;
    }
   ~m() { if (a) a->drop(); }
};

struct aitem {
    struct aitem* next;
    struct aitem* prev;
    m element; /// mx is still the intermediate class, since it holds onto and can debug any of the data you give it
    aitem(const m &e) : element(e) {
        next = 0;
        prev = 0;
    }
};

struct alist:A {
    aitem* first;
    aitem* last;
    int    count;
    alist() : A() {
        first = null;
        last  = null;
        count = 0;
    }
    template <typename T>
    alist(std::initializer_list<T> list) : alist() {
        for (auto i: list)
            push(i);
    }
    void remove(int index, int amount = 1) {
        int f = 0;
        bool found = false;
        for (aitem *i = first; i; i = i->next) {
            if (index == f++) {
                aitem *iprev = i->prev;
                for (int ii = 0; ii < amount; ii++) {
                    assert(i);
                    aitem *inext = i->next;
                    if (inext) inext->prev = iprev;
                    if (iprev) iprev->next = inext;
                    delete i;
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
    void operator += (const m &v) {
        push(v);
    }
};

struct afield {
    m key;
    m value; /// these are more debuggable than simple A* pointers
    afield() { }
    afield(const m &k, const m &v) {
        key   = (m&)k;
        value = (m&)v;
    }
};

struct amap:A {

};

/// it will be nice to make array use A classes ONLY, so we can simplify array
/// vector will be for primitive types, and we can assert error for A type

/// when A classes are created dynamically, their types will need to be set
#define decl(U, _A_) \
struct U { \
    private: \
    struct _A_* data; \
    public: \
    U(_A_* data) : data((_A_*)((A*)data)->hold()) { ((A*)data)->type = typeof(_A_); } \
    U(const U &b); \
    U(); \
    template <typename T> \
    U(std::initializer_list<T> list) : data(new _A_(list)) { ((A*)data)->type = typeof(_A_); } \
    template <typename... Arg> \
    U(Arg&&... arg) : data(new _A_(std::forward<Arg...>(arg...))) { ((A*)data)->type = typeof(_A_); } \
    template <typename T> U& operator *= (const T& v) { (*data) *= v; return *this; }\
    template <typename T> U& operator /= (const T& v) { (*data) /= v; return *this; }\
    template <typename T> U& operator += (const T& v) { (*data) += v; return *this; }\
    template <typename T> U& operator -= (const T& v) { (*data) -= v; return *this; }\
    template <typename T> U  operator *  (const T& v) { return (*data) * v; }\
    template <typename T> U  operator /  (const T& v) { return (*data) / v; }\
    template <typename T> U  operator +  (const T& v) { return (*data) + v; }\
    template <typename T> U  operator -  (const T& v) { return (*data) - v; }\
    operator m() const; \
    _A_* operator->() const; \
    operator _A_*() const; \
    U &operator=(const U &b); \
   ~U(); \
};

#define impl(U, _A_) \
    U::U(const U &b)      : data((_A_*)b.data->hold()) { } \
    U::U()                : data(new _A_())  { data->type = typeof(_A_); } \
    _A_* U::operator->   () const { return data; } \
         U::operator _A_*() const { return data; } \
    U   &U::operator=(const U &b) { \
        if (data != b.data) { \
            data->drop(); \
            data = (_A_*)b.data->hold(); \
        } \
        return *this; \
    } \
    U::operator m() const { return m(data); } \
    U::~U() { data->drop(); } \

decl(AUser, a_user)
impl(AUser, a_user)

decl(str2, string)
impl(str2, string)

decl(List, alist)
impl(List, alist)

/// positives:
/// no intern class, no schema
/// implement meta in one place, on the data
/// transitionable, once we figure out how to handle 'mx' here
/// mx would be a user class of a, a class of its own
/// inside it needs to be able to link to any sort of data, not just primitive

int main(int argc, char **argv) {
    AUser aclass;
    aclass->a = 10;
    aclass->b = 22;

    str2 a = "test"; a += "1";

    List l { str2("test"), str2("2") };

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