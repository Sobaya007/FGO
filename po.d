import std.stdio;
import std.algorithm;
import std.array;
import std.range;
import std.conv;
import std.string;

struct Field(uint N) {
    private int x;
    this(int x) {
        this.x = x % N;
    }

    Field opBinary(string op)(Field m) if (op != "/") {
        return Field(mixin("this.x " ~ op ~ " m.x + N") % N);
    }

    Field opBinary(string op)(Field m) if (op == "/") {
        return this * m.inverse;
    }

    Field opBinary(string op)(uint x) if (op == "^^") {
        return Field(this.x ^^ x);
    }

    void opOpAssign(string op)(Field m) {
        mixin("this.x = (this.x " ~ op ~ " m.x + N) % N;");
    }

    Field inverse() {
        return Field(this.x ^^ (N-2));
    }

    bool opEquals(int x) {
        return this.x == x;
    }

    bool opEquals(Field f) {
        return this.x == f.x;
    }

    string toString() {
        return x.to!string;
    }
}

struct Polynomial(uint N) {

    Field!N[] terms;

    this(int[] terms) {
        this.terms = terms.map!(term => Field!N(term)).array;
    }

    this(Field!N[] terms) {
        this.terms = terms;
    }

    Polynomial opBinaryRight(string op)(int n) if (op == "*") {
        auto f = Field!N(n);
        return Polynomial(terms.map!(term => term * f).array);
    }

    Polynomial opBinaryRight(string op)(int n) if (op != "*") {
        return Polynomial([Field!N(n) + terms[0]] ~ terms[1..$]);
    }

    Polynomial opBinary(string op)(uint d) if (op == "^^") {
        auto res = Polynomial([1]);
        foreach (i; 0..d) {
            res = res * this;
        }
        return res;
    }

    Polynomial opBinary(string op)(Field!N n) if (op == "*") {
        return Polynomial(terms.map!(term => term * n).array);
    }

    Polynomial opBinary(string op)(Polynomial p) if (op == "+" || op == "-") {
        Field!N[] terms;
        terms.length = max(this.terms.length, p.terms.length);
        this.terms.length = terms.length;
        p.terms.length = terms.length;
        foreach (i; 0..terms.length) {
            terms[i] = mixin("this.terms[i] " ~ op ~ " p.terms[i]");
        }
        return Polynomial(terms);
    }

    Polynomial opBinary(string op)(Polynomial p) if (op == "*") {
        Field!N[] terms;
        terms.length = this.terms.length + p.terms.length - 1;
        foreach (d1, t1; this.terms) {
            foreach (d2, t2; p.terms) {
                terms[d1+d2] += t1 * t2;
            }
        }
        return Polynomial(terms);
    }

    Polynomial opBinary(string op)(Polynomial p) if (op == "%") in {
        assert(p.terms[$-1] == 1);
    } body {
        auto res = this;

        while (res.dim >= p.dim) {
            res = res - Polynomial([0,1])^^(res.dim - p.dim) * res.terms[res.dim] * p;
        }
        return res;
    }

    bool opEquals(int x) {
        if (this.dim > 0) return false;
        return this.terms[0] == x;
    }

    bool opEquals(Polynomial p) {
        if (this.dim != p.dim) return false;
        foreach (i; 0..this.dim) {
            if (this.terms[i] != p.terms[i]) return false;
        }
        return true;
    }

    uint dim() {
        foreach_reverse(i; 0..this.terms.length) {
            if (this.terms[i] != 0) return i;
        }
        return 0;
    }

    string toString() {
        auto t = this.terms.dup;
        t.length = 3;
        return format!"[%s%s%s]"(t[0],t[1], t[2]);
        //if (terms.all!(term => term.x == 0)) return "0";
        //string[] strs;
        //foreach (dim, term; terms) {
        //    string ks, ds;
        //    if (term == 0) continue;
        //    if (term == 1 && dim == 0) {
        //        strs ~= "1";
        //        continue;
        //    }
        //    if (term == 1) {
        //        ks = "";
        //    } else {
        //        ks = term.to!string;
        //    }
        //    if (dim == 0) {
        //        ds = "";
        //    } else if (dim == 1) {
        //        ds = "X";
        //    } else {
        //        ds = "X^" ~ dim.to!string;
        //    }
        //    strs ~= ks ~ ds;
        //}
        //return strs.join(" + ");
    }
}

struct Quotient(uint M, alias poly) {

    alias P = Polynomial!M;

    private P myPoly;

    this(P p) {
        this.myPoly = p % poly;
    }

    Quotient opBinary(string op)(Quotient q) if (op != "/") {
        return Quotient(mixin("this.myPoly " ~ op ~ " q.myPoly"));
    }

    Quotient opBinary(string op)(Quotient q) if (op == "/") {
        return this * q.inverse;
    }

    Quotient opBinary(string op)(uint x) if (op == "^^") {
        return Quotient(this.myPoly ^^ x);
    }

    Quotient inverse() {
        enum X = P([0,1]);

        Quotient detect(int[] a) {
            if (a.length == poly.dim) {
                if (this.myPoly * P(a) % poly == 1) return Quotient(P(a));
                return Quotient(P([0]));
            }
            foreach (i; 0..M) {
                auto res = detect(a ~ i);
                if (res != 0) return res;
            }
            return Quotient(P([0]));
        }
        auto res = detect([]);
        assert(res != 0);
        return res;
    }

    bool opEquals(int x) {
        return myPoly == x;
    }

    bool opEquals(Quotient q) {
        return myPoly == q.myPoly;
    }

    string toString() {
        return myPoly.toString;
    }
}

struct Matrix(P,uint U, uint V) {

    P[U*V] elems;

    this(P[] elems) {
        foreach (i; 0..elems.length) {
            this.elems[i] = elems[i];
        }
    }

    void hakidashi() {
        foreach (i; 0..U) {
            writeln(this.toString);
            auto pivot = this[i,i];
            auto y = i;
            while (pivot == 0) {
                pivot = this[y,i];
                y++;
                if (y == U) {
                    writeln("ランクが足りません。");
                    return;
                }
            }
            if (i != y) {
                swapRow(i, y);
            }
            // 対角成分の選択、この値で行成分を正規化
            foreach (j; i..V) {
                this[i,j] = this[i,j] / pivot;
            }

            // 階段行列を作る為に、現在の行より下の行について
            // i列目の成分が0になるような基本変形をする
            foreach (k; 0..U) {
                if (k == i) continue;
                auto mul = this[k,i];
                foreach (n; i..V) {
                    this[k,n] = this[k,n] -  mul * this[i,n];
                }
            }
        }
        writeln(this.toString);
    }

    void swapRow(uint r1, uint r2) {
        foreach (j; 0..V) {
            auto tmp = this[r1, j];
            this[r1,j] = this[r2, j];
            this[r2,j] = tmp;
        }
    }

    string toString() {
        string res;
        foreach (i; 0..U) {
            foreach (j; 0..V) {
                res ~= this[i,j].toString;
            }
            res ~= "\n";
        }
        return res;
    }

    ref P opIndex(size_t y, size_t x) {
        return elems[V * y + x];
    }
}

void main() {
    writeln("問4.1");
    {
        enum M = 2;
        alias P = Polynomial!M;
        enum X = P([0,1]);
        enum Poly = 1 + X^^2 + X^^3;
        alias Q = Quotient!(M, Poly);
        Q Y(int x) {
            auto a = x / 100;
            auto b = x / 10 % 10;
            auto c = x % 10;
            return Q(a + b * X + c * X^^2);
        }

        writeln("(a)");
        auto left  = Y(110);
        auto right = Y( 11);
        auto ans   = left * right;
        writefln("%s x %s = %s", left, right, ans);

        writeln("(b)");
        auto m = Matrix!(Q,2,3)([
           Y(100), Y( 11), Y( 10),
           Y(100), Y(101), Y(101),
        ]);
        m.hakidashi();
    }
    writeln("問4.2");
    {
        enum M = 11;
        alias P = Polynomial!M;
        alias F = Field!M;
        writeln("(d)");
        auto a = [F(0), F(1), F(2), F(3), F(4)];
        auto r = [F(7), F(4), F(4), F(9), F(2)];
        F[] flist;
        foreach (i; 0..5) {
            foreach (j; 0..4) {
                flist ~= a[i]^^j;
            }
            foreach (j; 0..2) {
                flist ~= r[i]*a[i]^^j;
            }
        }
        auto m = Matrix!(F,5,6)(flist);
        writeln(m.toString);
        writeln("(e)");
        m.hakidashi();
    }
    writeln("問4.3");
    {
        enum M = 2;
        alias P = Polynomial!M;
        alias F = Field!M;
        enum X = P([0,1]);
        enum Poly = 1 + X^^2 + X^^3;
        alias Q = Quotient!(M, Poly);
        Q Z(int x) {
            auto a = x / 100;
            auto b = x / 10 % 10;
            auto c = x % 10;
            return Q(a + b * X + c * X^^2);
        }
        writeln("(d)");
        auto a = [Z(  0), Z(100), Z( 10), Z(110), Z(  1)];
        auto r = [Z(  0), Z(101), Z(110), Z( 11), Z(100)];
        Q[] flist;
        foreach (i; 0..5) {
            foreach (j; 0..4) {
                flist ~= a[i]^^j;
            }
            foreach (j; 0..2) {
                flist ~= r[i]*a[i]^^j;
            }
        }
        auto m = Matrix!(Q,5,6)(flist);
        writeln(m.toString);
        writeln("(e)");
        m.hakidashi();
    }
}
