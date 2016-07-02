/* idiotic microlisp */
/* PD / WTFPL */
import milf;


// ////////////////////////////////////////////////////////////////////////// //
class MilfExt : Milf {
  this () {
    super();
    this["file-exists?"] = (string fname) {
      import std.stdio;
      writeln("file-exists?: [", fname, "]");
      return false;
    };
    this["logln"] = (Milf eng, CellCons args) {
      import std.array : appender;
      auto ap = appender!string;
      while (!args.isNil) {
        ap.put(args.car.toNakedString);
        args = cast(CellCons)args.cdr;
      }
      import std.stdio;
      writeln("LOG: ", ap.data);
      return Cell.ctrue;
    };
    this["for-each-dir"] = (Milf eng, string path, Cell lmb) {
      auto lc = cast(CellLambda)lmb;
      if (lc is null) throw new MilfException("lambda expected for 'for-each-dir'", lmb);
      foreach (string s; ["dir0", "dir1", "wow"]) {
        bool done = false;
        try {
          eng.exec(lc, s);
        } catch (MilfBreakContException e) {
          done = e.isBreak;
        }
        if (done) break;
      }
      return Cell.ctrue;
    };
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  import std.stdio;

  auto eng = new MilfExt;

  Cell eval (string s) {
    //import core.exception : ExitException;
    try {
      auto c = eng.evaluate(s);
      return c;
    } catch (MilfParseException e) {
      writeln("PARSE ERROR ", e.loc, ": ", e.msg);
    } catch (MilfException e) {
      if (e.cell !is null && e.cell.loc.valid) {
        writeln("MILF ERROR ", e.cell.loc, ": ", e.msg);
      } else {
        writeln("MILF ERROR: ", e.msg);
      }
    } catch (Exception e) {
      writeln("ERROR: ", e.msg);
    }
    //throw new ExitException(1);
    throw new Exception("exit");
  }

  //eng.register("sum", (double a, double b=2) => a+b);
  eng.register("write", (string s=null) { import std.stdio; write(s); return true; });
  eng.register("writeln", (string s=null) { import std.stdio; writeln(s); return true; });
  if (args.length < 2) {
    eng["sum"] = (double a, double b=2) => a+b;
    {
      auto num = eng["sum"](40).to!double;
      writeln(typeof(num).stringof, " : ", num);
    }
    eval("(define (add a b) (+ a b)) (set! a 666)");
    {
      auto num = eng["add"](40, 2).to!double;
      writeln(typeof(num).stringof, " : ", num);
      writeln(eng["a"]);
    }
  } else if (args.length > 2 && args[1] == "-e") {
    foreach (/*auto*/ s; args[2..$]) {
      auto list = eval(s);
      writeln(list);
    }
  } else {
    foreach (/*auto*/ fname; args[1..$]) {
      // fuck readText, if wants utf
      string s;
      {
        import std.exception : assumeUnique;
        auto fl = File(fname);
        if (fl.size == 0) continue;
        if (fl.size > 1024*1024) throw new Exception("file '"~fname~"' too big");
        auto ctx = new char[](cast(usize)fl.size);
        fl.rawRead(ctx);
        s = ctx.assumeUnique;
      }
      writeln("=== ", fname, " ===");
      stdout.flush();
      auto list = eval(s);
      writeln(list);
    }
  }
}
