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

int main(int argc, char **argv) {
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