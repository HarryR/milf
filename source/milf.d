/* idiotic microlisp */
/* PD / WTFPL */
module milf /*is aliced*/;

alias usize = size_t;


// ////////////////////////////////////////////////////////////////////////// //
class MilfException : Exception {
  Cell cell;
  this (string message, Cell acell=null, string file=__FILE__, usize line=__LINE__, Throwable next=null) @safe pure nothrow {
    cell = acell;
    super(message, file, line, next);
  }
}


class MilfReturnException : Exception {
  Cell cell;
  this (Cell acell, string file=__FILE__, usize line=__LINE__, Throwable next=null) @trusted nothrow {
    cell = (acell !is null ? acell : Cell.cnil);
    super("return without function", file, line, next);
  }
}


class MilfBreakContException : Exception {
  Cell cell;
  bool isBreak;
  this (bool aIsBreak, Cell acell, string file=__FILE__, usize line=__LINE__, Throwable next=null) @trusted nothrow {
    cell = (acell !is null ? acell : Cell.cnil);
    isBreak = aIsBreak;
    super((isBreak ? "break without loop" : "continue without loop"), file, line, next);
  }
}


class MilfParseException : Exception {
  Cell.Loc loc;
  this (in Cell.Loc aloc, string message, string file=__FILE__, usize line=__LINE__, Throwable next=null) @safe pure nothrow {
    loc = aloc;
    super(message, file, line, next);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
bool isNil() (in Cell c) { return (c is null || c is Cell.cnil); }
bool isFalse() (in Cell c) { return (c is null || c is Cell.cnil || c is Cell.cfalse); }
bool isTrue() (in Cell c) { return !isFalse(c); }
string toString (in Cell c) { return (c !is null ? c.toString : "#nil"); }
string toNakedString (in Cell c) { return (c !is null ? c.toNakedString : ""); }


// ////////////////////////////////////////////////////////////////////////// //
class Cell {
  static struct Loc {
  align(1):
    usize cpos; // char postion
    ushort row;
    ushort col;

    @property bool valid () const {
      return (row > 0 && col > 0);
    }

    string toString () const {
      import std.string;
      return format("(%s,%s)", row, col);
    }
  }

  // special cells
  __gshared Cell ctrue = new Cell("#t");
  __gshared Cell cfalse = new Cell("#f");
  __gshared Cell cnil = new Cell("#nil");
  __gshared Cell ceof = new Cell("#eof");

  Loc loc; // start
  string name;

  this (in Loc aloc=Loc.init) { loc = aloc; }
  this (string aname="#dummy", in Loc aloc=Loc.init) { name = aname; loc = aloc; }
  // args is guaranteed to be either `null` or valid list
  Cell execute (Milf eng, CellCons args) { return this; }
  override string toString () const { return name; }
  string toNakedString () const { return this.toString; }

final:
  T to(T) () const {
    return Cell.to!T(this);
  }

  inout(Cell) opDispatch(string name) () inout
  if (name.length > 2 && name[0] == 'c' && name[$-1] == 'r' && isGoodCxROps!(name[1..$-1]))
  {
    return Cell.opDispatch!name(this);
  }

static:
  private template StripTypedef(T) {
    /*
    static if (is(T OT == typedef)) {
      alias StripTypedef = OT;
    } else {
      alias StripTypedef = T;
    }
    */
    alias StripTypedef = T;
  }

  private template isStringType(T, CT) {
    import std.traits;
    alias TT = StripTypedef!T;
    static if (isArray!TT) {
      static alias ArrayElementType(AT : AT[]) = Unqual!AT;
      enum isStringType = is(ArrayElementType!TT == CT);
    } else {
      enum isStringType = false;
    }
  }

  private enum isSomeStringType(T) = isStringType!(T, char) || isStringType!(T, wchar) || isStringType!(T, dchar);

  template isGoodType(T) {
    import std.traits;
    alias TT = StripTypedef!T;
    static if (isNumeric!TT ||
               isBoolean!TT ||
               isSomeChar!TT ||
               isSomeStringType!TT)
    {
      enum isGoodType = true;
    } else {
      enum isGoodType = false;
    }
  }

  template isGoodTypeOrCell(T) {
    import std.traits;
    alias TT = StripTypedef!T;
    static if (is(TT == Cell)) {
      enum isGoodTypeOrCell = true;
    } else {
      enum isGoodTypeOrCell = isGoodType!TT;
    }
  }

  Cell from(T) (T val) {
    import std.traits;
    alias TT = StripTypedef!T;
    static if (isNumeric!TT || isSomeChar!TT) {
      return new CellNum(cast(typeof(CellNum.n))val);
    } else static if (isBoolean!TT) {
      return (val ? Cell.ctrue : Cell.cfalse);
    } else static if (isStringType!(TT, char)) {
      return new CellStr(val);
    } else static if (isStringType!(TT, wchar) || isStringType!(TT, dchar)) {
      import std.conv : to;
      return new CellStr(to!string(val));
    } else {
      static assert(0, "can't convert type "~T.stringof~" to cell");
    }
  }

  T to(T) (in Cell cell) {
    import std.traits;
    alias TT = StripTypedef!T;
    if (cell is null) return to!T(Cell.cnil);
    static if (isNumeric!TT || isSomeChar!TT) {
      if (auto c = cast(const CellNum)cell) {
        return cast(TT)c.n;
      } else {
        throw new MilfException("can't convert cell to number");
      }
    } else static if (isBoolean!TT) {
      return cell.isTrue;
    } else static if (isSomeStringType!TT) {
      import std.conv : to;
      string str = cell.toNakedString;
      //if (auto c = cast(const CellStr)cell) str = c.s; else str = cell.toString;
      static if (isStringType!(TT, char)) return str;
      else static if (isStringType!(TT, wchar)) return to!wstring(str);
      else return to!dstring(str);
    } else {
      static assert(0, "can't convert type "~T.stringof~" to cell");
    }
  }

  inout(Cell) Car (inout Cell cell) {
    if (cell is null) {
      return cast(typeof(return))Cell.cnil;
    } else if (auto c = cast(CellCons)cell) {
      return cast(typeof(return))(c.mCar !is null ? c.mCar : Cell.cnil);
    } else {
      return cast(typeof(return))Cell.cnil;
    }
  }

  inout(Cell) Cdr (inout Cell cell) {
    if (cell is null) {
      return cast(typeof(return))Cell.cnil;
    } else if (auto c = cast(CellCons)cell) {
      return cast(typeof(return))(c.mCdr !is null ? c.mCdr : Cell.cnil);
    } else {
      return cast(typeof(return))Cell.cnil;
    }
  }

  private template isGoodCxROps(string s) {
    static if (s.length > 0) {
      static if (s[0] == 'a' || s[0] == 'd') {
        enum isGoodCxROps = isGoodCxROps!(s[1..$]);
      } else {
        enum isGoodCxROps = false;
      }
    } else {
      enum isGoodCxROps = true;
    }
  }

  inout(Cell) opDispatch(string name) (inout Cell cell)
  if (name.length > 2 && name[0] == 'c' && name[$-1] == 'r' && isGoodCxROps!(name[1..$-1]))
  {
    static string buildOps (string ops) {
      string s = `return `;
      foreach (char ch; ops) s ~= `Cell.C`~ch~`r(`;
      s ~= `cell`;
      foreach (___; ops) s ~= `)`;
      s ~= `;`;
      return s;
    }
    //enum op = buildOps(name[1..$-1]); pragma(msg, op);
    mixin(buildOps(name[1..$-1]));
  }

  enum usize BadList = usize.max;

  usize listLength (in Cell cell) {
    if (cell.isNil) return 0; // nil is always a good list
    if (auto hare = cast(CellCons)cell) { // it's ok to remove 'const' here
      auto tortoise = hare;
      bool tortStep = false;
      usize count = 0;
      while (!hare.isNil) {
        ++count; // one more cell
        if (hare.cdr.isNil) break; // end of list
        hare = cast(CellCons)hare.cdr;
        if (hare is null) return BadList; // this must be a cons
        if (hare is tortoise) return BadList; //throw new MilfException("endless list", null/*hare*/);
        if (tortStep) tortoise = cast(CellCons)tortoise.cdr;
        tortStep = !tortStep;
      }
      return count;
    }
    return BadList;
  }
}


//FIXME: this hack sux if we have `to` overloads in other modules!
T to(T, S) (S value) {
  static if (is(T : Cell)) {
    return Cell.from!S(value);
  } else static if (is(S : Cell)) {
    return Cell.to!T(value);
  } else {
    import std.conv : to;
    return to!T(value);
  }
}


version(milf_cell_test)
unittest {
  import std.stdio;
  {
    auto c = to!Cell(42.0);
    writefln("%s: %s", typeof(c).stringof, c.toString);
    auto n = to!int(c);
    writefln("%s: %s", typeof(n).stringof, n);
  }
}


class CellCons : Cell {
  Cell mCar, mCdr;

  this (Cell acar, Cell acdr, in Loc aloc=Loc.init) {
    super(aloc);
    mCar = (acar !is null ? acar : cnil);
    mCdr = (acdr !is null ? acdr : cnil);
  }

  this (Cell acar, Cell acdr) {
    this(acar, acdr, (acar !is null ? acar.loc : Loc.init));
  }

  final string doToString(string func) () const {
    import std.array : appender;
    auto res = appender!string;
    res.put("(");
    auto hare = cast(CellCons)this; // it's ok to remove 'const' here
    auto tortoise = hare;
    bool tortStep = false;
    while (!hare.isNil) {
      mixin("res.put(."~func~"(hare.car));");
      if (hare.cdr.isNil) break;
      if (auto d = cast(CellCons)hare.cdr) {
        res.put(" ");
        hare = d;
        if (hare is tortoise) { res.put("..."); break; }
        if (tortStep) tortoise = cast(CellCons)tortoise.cdr;
        tortStep = !tortStep;
      } else {
        res.put(" . ");
        mixin("res.put(."~func~"(hare.cdr));");
        break;
      }
    }
    res.put(")");
    return res.data;
  }

  override string toString () const { return doToString!"toString"(); }
  override string toNakedString () const { return doToString!"toNakedString"(); }
}

class CellNum : Cell {
  double n;

  this (double v=0.0, in Loc aloc=Loc.init) { super(aloc); n = v; }
  override string toString () const { import std.conv : to; return to!string(n); }
}

class CellStr : Cell {
  alias s = name;

  this (string v=null, in Loc aloc=Loc.init) { super(aloc); s = v; }

  override string toString () const {
    import std.array : appender;
    import std.format : formatElement, FormatSpec;
    auto res = appender!string();
    FormatSpec!char fspc; // defaults to 's'
    formatElement(res, name, fspc);
    return res.data;
  }
  override string toNakedString () const { return name; }
}

class CellSym : Cell {
  this (string n, in Loc aloc=Loc.init) { super(n, aloc); }
}

class CellPrimitive : Cell {
  this () { super("#primitive"); }
}

class CellPrimacro : CellPrimitive {
  // args is guaranteed to be either `null` or valid list
  alias Func = Cell function (Milf eng, CellCons args);
  Func fn;

  this (Func afn) { fn = afn; }
  override Cell execute (Milf eng, CellCons args) { return (fn !is null ? fn(eng, args) : cnil); }
  override string toString () const { import std.string : format; return format("#primacro%x", &fn); }
}

class CellPrim : CellPrimacro {
  this (Func afn) { super(afn); }
  override Cell execute (Milf eng, CellCons args) { return (fn !is null ? fn(eng, eng.evalArgs(args)) : cnil); }
  override string toString () const { import std.string : format; return format("#primitive%x", &fn); }
}

// lambda function
class CellLambda : Cell {
  CellCons arguments, bOdY;
  usize argCount;
  bool ismacro;

  this (Cell aargs, Cell abody, bool aismacro=false, in Loc aloc=Loc.init) {
    super(aloc);
    arguments = (aargs !is null ? cast(CellCons)aargs : null);
    bOdY = (abody !is null ? cast(CellCons)abody : null);
    ismacro = aismacro;
    argCount = Cell.listLength(arguments);
    if (argCount == Cell.BadList) throw new MilfException("bad function argument list");
  }

  override Cell execute (Milf eng, CellCons args) {
    auto acount = Cell.listLength(args);
    if (acount == Cell.BadList) throw new MilfException("bad function argument list", args);
    if (acount < argCount) throw new MilfException("out of arguments", args);
    if (acount > argCount) throw new MilfException("too many arguments", args);
    if (!ismacro) args = eng.evalArgs(args);
    eng.pushFrame(arguments, args);
    scope(exit) eng.popFrame();
    try {
      return eng.evalProg(bOdY);
    } catch (MilfReturnException re) {
      return re.cell;
    }
  }

  override string toString() const { return "(lambda "~.toString(arguments)~" "~.toString(bOdY)~")"; }
}


// ////////////////////////////////////////////////////////////////////////// //
class Milf {
  import std.typecons : Flag, Yes, No;


  Cell[string] globals; // symbol values
  Cell[string][] frames;

  this () { registerBuiltins(); }

final:
  // ////////////////////////////////////////////////////////////////////// //
  static void getArgs (string name, Cell[] its, Cell args, usize min=usize.max) {
    if (min == usize.max) min = its.length;
    usize pos = 0;
    Cell list = args;
    while (pos < its.length) {
      if (list.isNil) break;
      if (auto cons = cast(CellCons)list) {
        if (!cons.cdr.isNil && cast(CellCons)cons.cdr is null) throw new MilfException("invalid arglist for '"~name~"'", args);
        its[pos++] = cons.car;
        list = cons.cdr;
      } else {
        throw new MilfException("invalid arglist for '"~name~"'", args);
      }
    }
    if (!list.isNil) throw new MilfException("arglist too long for '"~name~"'", args);
    if (pos < min) throw new MilfException("arglist too short for '"~name~"'", args);
    if (pos < its.length) its[pos..$] = null;
  }

  // ////////////////////////////////////////////////////////////////////// //
  void pushFrame (CellCons names=null, CellCons values=null) {
    auto fidx = frames.length;
    frames.length = fidx+1;
    while (!names.isNil) {
      if (auto csym = cast(CellSym)names.car) {
        if (!values.isNil) {
          frames[fidx][csym.name] = values.car;
        } else {
          frames[fidx][csym.name] = Cell.cnil;
        }
      } else {
        throw new MilfException("invalid name", names.car);
      }
      names = cast(CellCons)names.cdr;
      values = cast(CellCons)values.cdr;
    }
  }

  void popFrame () {
    if (frames.length == 0) throw new MilfException("out of frames");
    frames.length = frames.length-1;
  }

  // ////////////////////////////////////////////////////////////////////// //
  // null: no variable found in any frame
  Cell *findVar (string name) nothrow @nogc {
    if (name.length == 0) {
      return null;
    } else {
      foreach_reverse (ref frame; frames) if (auto cell = name in frame) return cell;
      return (name in globals);
    }
  }

  // null: no variable found in any frame
  Cell getVar (string name) nothrow {
    if (auto var = findVar(name)) {
      return *var;
    } else {
      return null;
    }
  }

  // value=null: remove variable from topmost frame
  void setVar (string name, Cell value, Flag!"AddMissing" addMissing=No.AddMissing) {
    if (name.length == 0) throw new MilfException("variable '' can't be changed");
    if (value is null) {
      // remove variable
      if (frames.length) frames[$-1].remove(name); else globals.remove(name);
      return;
    }
    if (auto var = findVar(name)) {
      *var = value;
    } else {
      if (addMissing) {
        if (frames.length) {
          frames[$-1][name] = value;
        } else {
          globals[name] = value;
        }
      } else {
        throw new MilfException("variable '"~name~"' not found");
      }
    }
  }

  void setTopVar (string name, Cell value) {
    if (name.length == 0) throw new MilfException("variable '' can't be changed");
    if (value is null) value = Cell.cnil;
    if (frames.length) frames[$-1][name] = value; else globals[name] = value;
  }

  auto opIndex (string name) {
    if (auto var = findVar(name)) {
      //import std.traits : isSomeFunction;
      struct RetVar {
        Milf eng;
        Cell var;

        @disable this ();
        private this (Milf e, Cell c) {
          eng = e;
          var = c;
        }
        Cell opCall(Args...) (Args arguments) {
          return eng.exec(var, arguments);
        }
        alias var this;
      }
      return RetVar(this, *var);
    } else {
      throw new MilfException("variable '"~name~"' not found");
    }
  }

  Cell opIndexAssign (Cell value, string name) {
    if (value is null) value = Cell.cnil;
    //setVar(name, value, Yes.AddMissing);
    setTopVar(name, value);
    return value;
  }

  Cell exec(Args...) (Cell cell, Args arguments) {
    // build arglist
    CellCons alist = null, cur = null;
    foreach (immutable idx, immutable arg; arguments) {
      auto c = Cell.from(arg);
      if (alist is null) {
        alist = cur = new CellCons(c, Cell.cnil);
      } else {
        cur.mCdr = new CellCons(c, Cell.cnil);
        cur = cast(CellCons)cur.mCdr;
      }
    }
    return cell.execute(this, alist);
  }

  // ////////////////////////////////////////////////////////////////////// //
  Cell eval (Cell cell) {
    if (cell.isNil) return Cell.cnil;
    if (auto cons = cast(CellCons)cell) {
      // this seems to be a list
      if (Cell.listLength(cons) == Cell.BadList || cons.mCar is null) throw new MilfException("bad list", cell);
      auto head = cons.mCar;
      auto tail = cast(CellCons)cons.cdr;
      // if head is the list, evaluate it first
      if (cast(CellCons)head !is null) head = eval(head);
      if (auto sym = cast(CellSym)head) {
        // resolve symbol
        if (auto var = findVar(sym.name)) {
          if (*var is null) throw new MilfException("can't execute '"~sym.name~"'", cell);
          auto res = (*var).execute(this, tail);
          if (res is null) res = Cell.cnil; // fix common error
          return res;
        } else {
          throw new MilfException("unknown variable '"~sym.name~"'", cell);
        }
      } else if (cast(CellLambda)head !is null || cast(CellPrimitive)head !is null) {
        return head.execute(this, cast(CellCons)cons.cdr);
      } else {
        import std.stdio : writefln; writefln("%s %s", typeof(head).stringof, cell);
        throw new MilfException("can't evaluate list", cell);
      }
    } else if (auto sym = cast(CellSym)cell) {
      // symbol, get it's value
      if (auto var = findVar(sym.name)) {
        return (*var !is null ? *var : Cell.cnil);
      } else {
        throw new MilfException("unknown variable '"~sym.name~"'", cell);
      }
    } else {
      // other cell types evaluates to themselves
      return cell;
    }
  }

  CellCons evalArgs (CellCons args) {
    if (!args.isNil) {
      return new CellCons(eval(args.car), evalArgs(cast(CellCons)args.cdr));
    } else {
      return null; // can't return CellCons here
    }
  }

  // ////////////////////////////////////////////////////////////////////// //
  import std.range : isInputRange;
  // returns `null` on eof
  Cell parseOne(R) (auto ref R ir, ref Cell.Loc loc) if (isInputRange!R) {
    Cell.Loc stloc;
    import std.array : appender;
    auto ap = appender!string;

    bool eof () { return ir.empty; }
    char curChar () { return (!ir.empty ? ir.front : 0); }
    void skipChar () {
      if (!ir.empty) {
        ++loc.cpos;
        ++loc.col;
        if (ir.front == '\n') {
          ++loc.row;
          loc.col = 1;
        }
        ir.popFront();
      }
    }

    static bool isSpecial (char ch) {
      return
        (ch == '(' || ch == ')' ||
         ch == '[' || ch == ']' ||
         ch == '{' || ch == '}' ||
         ch == ';');
    }

    void skipSpaces () {
      while (!eof) {
        char ch = curChar();
        if (ch == ';') {
          while (!eof && curChar() != '\n') skipChar();
        } else {
          if (ch > ' ') break;
        }
        skipChar();
      }
    }

    string collectId () {
      while (!eof) {
        char ch = curChar();
        if (ch <= ' ' || isSpecial(ch)) break;
        ap.put(ch);
        skipChar();
      }
      return ap.data;
    }

    Cell parseId () {
      return new CellSym(collectId(), stloc);
    }

    Cell parseNum () {
      bool wasDot = false;
      while (!eof) {
        char ch = curChar();
        if (ch == '.') {
          if (wasDot) return parseId();
          wasDot = true;
          ap.put(".");
          skipChar();
          ch = curChar();
          if (ch < '0' || ch > '9') return parseId();
        } else if (ch < '0' || ch > '9') {
          break;
        } else {
          ap.put(ch);
          skipChar();
        }
      }
      if (!eof && !isSpecial(curChar()) && curChar() > ' ') {
        // not a number
        return parseId();
      } else {
        // number
        import std.conv : to;
        return new CellNum(to!double(ap.data), stloc);
      }
    }

    // open quote eaten
    Cell parseString (Flag!"ParseEscapes" doEscapes) {
      for (;;) {
        if (eof) throw new MilfParseException(stloc, "unterminated string");
        char ch = curChar();
        skipChar();
        if (ch == '"') {
          if (doEscapes || curChar() != '"') break;
          ap.put(ch);
          skipChar();
        } else if (doEscapes && ch == '\\') {
          if (eof) throw new MilfParseException(stloc, "unterminated string");
          ch = curChar();
          skipChar();
          switch (ch) {
            case '\\': case '"': case '\'': case '`': ap.put(ch); break;
            case 't': ap.put("\t"); break;
            case 'n': ap.put("\n"); break;
            case 'r': ap.put("\r"); break;
            case 'x': case 'X': throw new MilfParseException(loc, "hex string escapes are not here yet");
            case 'u': case 'U': throw new MilfParseException(loc, "unicode hex string escapes are not here yet");
            default: throw new MilfParseException(loc, "invalid string escape");
          }
        } else {
          ap.put(ch);
        }
      }
      if (!eof && !isSpecial(curChar()) && curChar > ' ') throw new MilfParseException(stloc, "invalid string");
      return new CellStr(ap.data, stloc);
    }

    Cell parseQuote () {
      auto qc = new CellSym("quote", stloc);
      auto res = parseOne(ir, loc);
      if (res is null) throw new MilfParseException(stloc, "invalid quoting");
      res = new CellCons(res, Cell.cnil);
      return new CellCons(qc, res);
    }

    Cell parseList (char ch) {
      ch = (ch == '(' ? ')' : ']');
      CellCons res = null, cur = null;
      for (;;) {
        skipSpaces();
        if (eof) throw new MilfParseException(stloc, "unfinished list");
        if (curChar() == ch) break;
        if (curChar() == '.') {
          // last item is dotted pair
          if (cur is null) throw new MilfParseException(loc, "invalid cons");
          skipChar();
          auto cell = parseOne(ir, loc);
          if (cell is null) throw new MilfParseException(stloc, "unfinished list");
          skipSpaces();
          if (curChar() != ch) throw new MilfParseException(stloc, "unfinished list");
          cur.mCdr = cell;
          break;
        } else {
          auto cell = parseOne(ir, loc);
          if (cell is null) throw new MilfParseException(stloc, "unfinished list");
          auto cons = new CellCons(cell, Cell.cnil);
          if (cur is null) {
            res = cur = cons;
            res.loc = stloc;
          } else {
            cur.mCdr = cons;
            cur = cons;
          }
        }
      }
      skipChar(); // skip closing bracket
      return (res !is null ? res : Cell.cnil);
    }

    Cell parseSpecial () {
      if (eof) throw new MilfParseException(stloc, "invalid special");
      switch (/*auto s =*/ collectId()) {
        case "t": return Cell.ctrue;
        case "f": return Cell.cfalse;
        case "nil": return Cell.cnil;
        case "eof": return Cell.ceof;
        default: throw new MilfParseException(stloc, "invalid special '#"~/*s~*/"'");
      }
      assert(0);
    }

    Cell parseHereDoc () {
      char[4] cc;
      while (!eof && curChar() != '\n') skipChar();
      skipChar();
      cc[0] = curChar();
      skipChar();
      cc[1] = curChar();
      skipChar();
      cc[2] = curChar();
      skipChar();
      for (;;) {
        cc[3] = curChar();
        skipChar();
        if ((cc[3] == '\n' || cc[3] == '\r') && cc[0..3] == ">>>") {
          return new CellStr(ap.data, stloc);
        }
        if (eof) throw new MilfParseException(stloc, "unfinished heredoc");
        ap.put(cc[0]);
        cc[0] = cc[1];
        cc[1] = cc[2];
        cc[2] = cc[3];
      }
    }

    // fix initial position
    if (loc.row == 0) loc.row = 1;
    if (loc.col == 0) loc.col = 1;
    // endless cycle to avoid recursion on comments
    // all non-comment parsers does `return`
    for (;;) {
      skipSpaces();
      if (eof) return null;
      stloc = loc;
      char ch = curChar();
      skipChar();
      switch (ch) {
        case '\'': // quote
          return parseQuote();
        case '(': case '[':
          return parseList(ch);
        case '<': // heredoc?
          if (curChar() == '<') {
            skipChar(); // skip second
            if (curChar() == '<') {
              // almost real heredoc
              skipChar(); // skip third
              if (curChar() == '\n' || curChar() == '\r') {
                return parseHereDoc();
              }
              ap.put('<');
            }
            ap.put('<');
          }
          ap.put('<');
          return parseId();
        case '#': // special
          /*
          if (curChar == '(' || curChar == '[') {
            // comment
            { import std.stdio; writeln(loc, " : ", curChar()); }
            parseOne(ir, loc);
            // parse next datum
            { import std.stdio; writeln(loc, " : ", curChar()); }
          } else {
            return parseSpecial();
          }
          break;
          */
          return parseSpecial();
        case '+': case '-':
          ap.put(ch);
          if (curChar() < '0' || curChar > '9') return parseId();
          return parseNum();
        case '0': .. case '9':
          ap.put(ch);
          return parseNum();
        case '"':
          return parseString(Yes.ParseEscapes);
        case 'r':
          // id or r"..."
          if (curChar() == '"') {
            skipChar();
            return parseString(No.ParseEscapes);
          }
          ap.put(ch);
          return parseId();
        default:
          if (!isSpecial(ch)) {
            // identifier
            ap.put(ch);
            return parseId();
          }
          throw new MilfParseException(stloc, "unexpected char");
      }
    }
  }

  Cell evaluate(R) (auto ref R ir) if (isInputRange!R) {
    Cell res = Cell.cnil;
    Cell.Loc loc;
    for (;;) {
      auto cell = parseOne(ir, loc);
      if (cell is null) break;
      res = eval(cell);
    }
    return res;
  }

  Cell evaluate (string s) {
    import std.utf : byChar;
    return evaluate(s.byChar);
  }

  Cell evalProg (Cell lst) {
    auto res = Cell.cnil;
    if (auto cur = cast(CellCons)lst) {
      while (!cur.isNil) {
        res = eval(cur.car);
        cur = cast(CellCons)cur.cdr;
      }
    }
    return res;
  }

  // ////////////////////////////////////////////////////////////////////// //
  private static Cell doMath(string op) (CellCons args) {
    if (args.cdr.isNil) throw new MilfException("invalid math call", args);
    auto res = Cell.to!double(args.car);
    for (Cell c = args.cdr; !c.isNil; c = c.cdr) {
      auto o = Cell.to!double(c.car);
      // division by zero?
      static if (op == "/") {
        if (o == 0.0) throw new MilfException("division by zero", args);
      }
      mixin(`res `~op~`= o;`);
    }
    return Cell.from(res);
  }

  // ////////////////////////////////////////////////////////////////////// //
  private enum doRegister = q{
    import std.traits : ParameterTypeTuple, ParameterDefaultValueTuple, ReturnType;
    if (name.length < 1 || (name[0] == ' ' && name.length < 2)) throw new MilfException("invalid name");
    ParameterTypeTuple!T arguments;
    Cell cfn;
    static if (is(ReturnType!T == Cell) && arguments.length == 2 &&
               is(typeof(arguments[0]) == Milf) && is(typeof(arguments[1]) == CellCons))
    { // register primitive
      if (name[0] == ' ') {
        name = name[1..$];
        cfn = new CellPrimacro(dg);
      } else {
        cfn = new CellPrim(dg);
      }
    } else {
      static assert(is(ReturnType!T == void) || Cell.isGoodTypeOrCell!(ReturnType!T), "invalid function return type "~(ReturnType!T).stringof);
      // have to do it here, as default argument values is not a part of the type
      foreach (immutable idx, immutable arg; arguments) {
        alias argType = typeof(arg);
        static if (idx == 0 && is(argType : Milf)) {
        } else {
          static assert(Cell.isGoodTypeOrCell!argType, "invalid function argument #"~to!string(idx)~" type");
        }
      }
      alias defaultArguments = ParameterDefaultValueTuple!dg;
      auto cc = new CellFnX!(T, defaultArguments)((name[0] == ' '), dg);
      if (cc.ismacro) name = name[1..$];
      cfn = cc;
    }
    this[name] = cfn;
    //globals[name] = cfn;
    return cfn;
  };
  import std.traits : isSomeFunction;
  Cell register(T) (string name, T dg) if (isSomeFunction!T) {
    mixin(doRegister);
  }

  Cell opIndexAssign(T) (T dg, string name) if (isSomeFunction!T) {
    mixin(doRegister);
  }

  // ////////////////////////////////////////////////////////////////////// //
  void registerBuiltins () {
    this[" quote"] = (Milf eng, CellCons args) {
      if (!args.cdr.isNil) throw new MilfException("too many args to 'quote'", args);
      return args.car;
    };
    this[" if"] = (Milf eng, CellCons args) {
      Cell[3] ctf;
      eng.getArgs("if", ctf[], args, 2);
      if (ctf[2] is null) ctf[2] = Cell.ctrue;
      return eng.eval(ctf[isTrue(eng.eval(ctf[0])) ? 1 : 2]);
    };
    this[" cond"] = (Milf eng, CellCons args) {
      while (!args.isNil) {
        auto cond = eng.eval(args.car);
        if (cond.isTrue) {
          return eng.eval(args.cdr.car);
        } else {
          args = cast(CellCons)args.cdr.cdr;
        }
      }
      return Cell.cfalse;
    };
    // logic
    this["not"] = (Milf eng, CellCons args) {
      if (Cell.listLength(args) != 1) throw new Exception("invalid number of arguments for 'not'");
      return Cell.from(args.car.isFalse);
    };
    this[" or"] = (Milf eng, CellCons args) {
      while (!args.isNil) {
        auto c = eng.eval(args.car);
        if (c.isTrue) return c;
        args = cast(CellCons)args.cdr;
      }
      return Cell.cfalse;
    };
    this[" and"] = (Milf eng, CellCons args) {
      auto res = Cell.cfalse;
      while (!args.isNil) {
        res = eng.eval(args.car);
        if (res.isFalse) return Cell.cfalse;
        args = cast(CellCons)args.cdr;
      }
      return res;
    };
    // vars
    this[" set"] = (Milf eng, CellCons args) {
      Cell[2] ctf;
      eng.getArgs("set", ctf[], args);
      if (auto sym = cast(CellSym)ctf[0]) {
        eng.setVar(sym.name, ctf[1], Yes.AddMissing);
        return ctf[1];
      } else {
        throw new MilfException("invalid 'set!'", args);
      }
    };
    this[" set!"] = (Milf eng, CellCons args) {
      Cell[2] ctf;
      eng.getArgs("set!", ctf[], args);
      if (auto sym = cast(CellSym)ctf[0]) {
        ctf[1] = eng.eval(ctf[1]);
        eng.setVar(sym.name, ctf[1], Yes.AddMissing);
        return ctf[1];
      } else {
        throw new MilfException("invalid 'set!'", args);
      }
    };
    this[" define"] = (Milf eng, CellCons args) {
      auto a0 = Cell.car(args);
      if (a0.isNil) throw new MilfException("invalid name for 'define'");
      if (Cell.cdr(args).isNil) throw new MilfException("invalid bOdY for 'define'");
      if (auto hdr = cast(CellCons)a0) {
        // (define (name args) bOdY)
        if (Cell.listLength(hdr) == Cell.BadList) throw new MilfException("invalid header for 'define'");
        //{ import std.stdio : writeln; writeln(hdr); }
        for (Cell c = hdr; !c.isNil; c = Cell.cdr(c)) {
          //{ import std.stdio : writeln; writeln(Cell.car(c)); }
          if (cast(CellSym)Cell.car(c) is null) throw new MilfException("invalid header for 'define'");
        }
        auto name = cast(CellSym)Cell.car(hdr);
        if (name.name.length == 0) throw new MilfException("invalid variable name for 'define'");
        auto lmb = new CellLambda(Cell.cdr(hdr), Cell.cdr(args));
        eng.setTopVar(name.name, lmb);
        return a0;
      } else if (auto name = cast(CellSym)a0) {
        // (define name value)
        if (name.name.length == 0) throw new MilfException("invalid variable name for 'define'");
        if (!Cell.cddr(args).isNil) throw new MilfException("too big bOdY for 'define'");
        auto v = eng.eval(Cell.cadr(args));
        eng.setTopVar(name.name, v);
        return a0;
      } else {
        throw new MilfException("invalid 'define'");
      }
    };
    // (lambda (args) bOdY) --> CellLambda
    this[" lambda"] = (Milf eng, CellCons args) {
      auto a0 = args.car;
      //if (a0.isNil) throw new MilfException("invalid args for 'lambda'");
      auto a1 = args.cdr;
      if (a1.isNil) throw new MilfException("invalid bOdY for 'lambda'");
      if (!a0.isNil) {
        // has args
        if (auto hdr = cast(CellCons)a0) {
          if (Cell.listLength(hdr) == Cell.BadList) throw new MilfException("invalid header for 'lambda'");
          for (Cell c = hdr; !c.isNil; c = Cell.cdr(c)) {
            if (cast(CellSym)Cell.car(c) is null) throw new MilfException("invalid header for 'lambda'");
          }
          auto lmb = new CellLambda(hdr, a1);
          return cast(Cell)lmb;
        } else {
          throw new MilfException("invalid header for 'lambda'");
        }
      } else {
        // no args
        auto lmb = new CellLambda(null, a1);
        return cast(Cell)lmb;
      }
    };
    this["car"] = (Milf eng, CellCons args) {
      Cell[1] ctf;
      eng.getArgs("car", ctf[], args);
      return Cell.car(ctf[0]);
    };
    this["cdr"] = (Milf eng, CellCons args) {
      Cell[1] ctf;
      eng.getArgs("cdr", ctf[], args);
      return Cell.cdr(ctf[0]);
    };
    this["list"] = (Milf eng, CellCons args) => cast(Cell)args;
    this["length"] = (Milf eng, CellCons args) {
      Cell[1] ctf;
      eng.getArgs("length", ctf[], args);
      auto len = Cell.listLength(ctf[0]);
      if (len == Cell.BadList) return Cell.cfalse;
      return new CellNum(len);
    };
    // math
    this["+"] = (Milf eng, CellCons args) => eng.doMath!"+"(args);
    this["-"] = (Milf eng, CellCons args) => eng.doMath!"-"(args);
    this["*"] = (Milf eng, CellCons args) => eng.doMath!"*"(args);
    this["/"] = (Milf eng, CellCons args) => eng.doMath!"/"(args);
    this["%"] = (Milf eng, CellCons args) => eng.doMath!"%"(args);
    // comparisons
    this["<"] = (double a, double b) => a < b;
    this[">"] = (double a, double b) => a > b;
    this["<="] = (double a, double b) => a <= b;
    this[">="] = (double a, double b) => a >= b;
    this["="] = (double a, double b) => a == b;
    this["<>"] = (double a, double b) => a != b;
    // string comparison
    this["$<"] = (string a, string b) => a < b;
    this["$>"] = (string a, string b) => a > b;
    this["$<="] = (string a, string b) => a <= b;
    this["$>="] = (string a, string b) => a >= b;
    this["$="] = (string a, string b) => a == b;
    this["$<>"] = (string a, string b) => a != b;
    // strings
    this["$+"] = (Milf eng, CellCons args) {
      import std.array : appender;
      if (args is null) return cast(Cell)(new CellStr(""));
      auto ap = appender!string;
      while (!args.isNil) {
        ap.put(args.car.toNakedString);
        args = cast(CellCons)args.cdr;
      }
      return new CellStr(ap.data);
    };
    this["$->sym"] = (Milf eng, CellCons args) {
      if (args.isNil) throw new MilfException("out of args for '$->sym'", args);
      string str;
      if (args.cdr.isNil) {
        // one arg
        str = Cell.to!string(args.car);
      } else {
        // many args
        import std.array : appender;
        if (args is null) return cast(Cell)(new CellStr(""));
        auto ap = appender!string;
        while (!args.isNil) {
          ap.put(args.car.toNakedString);
          args = cast(CellCons)args.cdr;
        }
        str = ap.data;
      }
      switch (str) {
        case "#nil": return Cell.cnil;
        case "#t": return Cell.ctrue;
        case "#f": return Cell.cfalse;
        case "#eof": return Cell.ceof;
        default: return cast(Cell)(new CellSym(str));
      }
      assert(0);
    };
    this["$length"] = (string s) => s.length;
    // predicates
    this["nil?"] = (Milf eng, CellCons args) => Cell.from!bool(args.car.isNil && args.cdr.isNil);
    this["number?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellNum)(args.car) !is null));
    this["string?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellStr)(args.car) !is null));
    this["symbol?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellSym)(args.car) !is null));
    this["primitive?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellPrimitive)(args.car) !is null));
    this["lambda?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellLambda)(args.car) !is null));
    this["cons?"] = (Milf eng, CellCons args) => Cell.from!bool(!args.isNil && (cast(CellCons)(args.car) !is null));
    // misc
    this["begin"] = (Milf eng, CellCons args) => eng.evalProg(args);
    // exceptions (alike)
    this["finally"] = (Milf eng, CellCons args) {
      Cell[2] ctf;
      eng.getArgs("finally", ctf[], args);
      scope(exit) eng.eval(ctf[0]);
      return eng.eval(ctf[1]);
    };
    this["throw"] = (Milf eng, CellCons args) {
      if (false) return Cell.cnil; // set return type
      Cell[2] ctf;
      eng.getArgs("throw", ctf[], args, 1);
      throw new MilfException(ctf[0].toNakedString, ctf[1]);
    };
    // flow control
    this["return"] = (Milf eng, CellCons args) {
      if (false) return Cell.cnil; // set return type
      Cell[1] ctf;
      eng.getArgs("return", ctf[], args, 0);
      throw new MilfReturnException(ctf[0]);
    };
    this["break"] = (Milf eng, CellCons args) {
      if (false) return Cell.cnil; // set return type
      Cell[1] ctf;
      eng.getArgs("break", ctf[], args, 0);
      throw new MilfBreakContException(true, ctf[0]);
    };
    this["continue"] = (Milf eng, CellCons args) {
      if (false) return Cell.cnil; // set return type
      Cell[1] ctf;
      eng.getArgs("continue", ctf[], args, 0);
      throw new MilfBreakContException(false, ctf[0]);
    };
    // loops
    this[" while"] = (Milf eng, CellCons args) {
      auto len = Cell.listLength(args);
      if (len == Cell.BadList || len < 2) throw new MilfException("invalid arglist for 'while'");
      auto cond = args.car;
      auto bOdY = args.cdr;
      auto res = Cell.cnil;
      bool done = false;
      while (!done) {
        if (!eng.eval(cond).isTrue) break;
        try {
          res = eng.evalProg(args);
        } catch (MilfBreakContException e) {
          res = e.cell;
          done = (e.isBreak);
        }
      }
      return res;
    };
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class CellFn : CellPrimitive {
  bool ismacro;

  this (bool aismacro) { ismacro = aismacro; }
  override Cell execute (Milf eng, CellCons args) {
    if (!ismacro) args = eng.evalArgs(args);
    return Cell.cnil;
  }
}

private class CellFnX(T, Defs...) : CellFn {
  T dg;

  this(T) (bool aismacro, T adg) { super(aismacro); dg = adg; }

  override Cell execute (Milf eng, CellCons args) {
    if (!ismacro) args = eng.evalArgs(args);
    if (dg is null) return Cell.cnil;

    import std.traits : ParameterTypeTuple, ReturnType;
    // prepare arguments
    ParameterTypeTuple!T arguments;
    foreach (/*auto*/ idx, ref arg; arguments) {
      // populate arguments, with user data if available,
      // default if not, and throw if no argument provided
      alias argType = typeof(arg);
      static if (idx == 0 && is(argType : Milf)) {
        arg = eng;
      } else {
        if (!args.isNil) {
          static if (is(argType == Cell)) {
            arg = args.car;
          } else {
            arg = Cell.to!argType(args.car);
          }
          args = cast(CellCons)args.cdr;
        } else {
          static if (!is(Defs[idx] == void)) {
            arg = Defs[idx];
          } else {
            import std.conv : to;
            throw new Exception("Required argument #"~to!string(idx)~" is missing.");
          }
        }
      }
    }
    if (!args.isNil) throw new Exception("too many args");
    // call function, convert return type
    alias retType = ReturnType!T;
    static if (is(ReturnType!T == void)) {
      dg(arguments);
      return Cell.cnil;
    } else {
      auto res = dg(arguments);
      static if (is(ReturnType!T == Cell)) {
        return res;
      } else {
        return Cell.from(res);
      }
    }
  }
}
