reader = require("reader");
compiler = require("compiler");
ac = function (x, env) {
  env = env || [];
  if (ac_string63(x)) {
    return(ac_string(x, env));
  } else {
    if (ac_literal63(x)) {
      return(x);
    } else {
      if (x === "nil") {
        return(["quote", "nil"]);
      } else {
        if (ac_symbol63(x)) {
          return(ac_var_ref(x, env));
        } else {
          if (xcar(x) === "quote") {
            return(["quote", ac_niltree(cadr(x))]);
          } else {
            if (xcar(x) === "if") {
              return(ac_if(cdr(x), env));
            } else {
              if (xcar(x) === "fn") {
                return(ac_fn(cadr(x), cddr(x), env));
              } else {
                if (xcar(x) === "assign") {
                  return(ac_set(cdr(x), env));
                } else {
                  if (! atom63(x)) {
                    return(ac_call(car(x), cdr(x), env));
                  } else {
                    throw new Error("Bad object in expression " + string(x));
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};
ac_symbol63 = function (x) {
  return(string63(x));
};
ac_string63 = function (x) {
  return(string_literal63(x));
};
ac_string = function (x, env) {
  return(x);
};
ac_literal63 = function (x) {
  return(boolean63(x) || ac_string63(x) || number63(x) || ! atom63(x) && none63(x));
};
xcar = function (x) {
  if (! atom63(x)) {
    return(hd(x));
  }
};
xcdr = function (x) {
  if (! atom63(x)) {
    return(tl(x));
  }
};
dot = unique("dot");
car = function (x) {
  if (! x) {
    return(undefined);
  } else {
    if (atom63(x)) {
      throw new Error("car: expected list, got " + string(x));
    } else {
      if (none63(x)) {
        return(undefined);
      } else {
        var v = hd(x);
        if (v === dot) {
          throw new Error("car: bad cons " + string(x));
        } else {
          return(v);
        }
      }
    }
  }
};
cdr = function (x) {
  if (! x) {
    return(undefined);
  } else {
    if (atom63(x)) {
      throw new Error("cdr: expected list, got " + string(x));
    } else {
      if (none63(x)) {
        return(undefined);
      } else {
        var v = tl(x);
        if (xcar(v) === dot) {
          if (! one63(tl(v))) {
            throw new Error("cdr: bad cons " + string(x));
          } else {
            return(hd(tl(v)));
          }
        } else {
          return(v);
        }
      }
    }
  }
};
cadr = function (x) {
  return(car(cdr(x)));
};
cddr = function (x) {
  return(cdr(cdr(x)));
};
cons = function (x, y) {
  if (atom63(y)) {
    if (y) {
      return([x, dot, y]);
    } else {
      return([x]);
    }
  } else {
    return(join([x], y));
  }
};
pair63 = function (x) {
  return(! atom63(x) && ! function63(x));
};
null63 = function (x) {
  return(! is63(x) || pair63(x) && none63(x));
};
ac_if = function (args, env) {
  if (null63(args)) {
    return(["quote", "nil"]);
  } else {
    if (null63(cdr(args))) {
      return(ac(car(args), env));
    } else {
      return(["if", ["not", ["ar-false?", ac(car(args), env)]], ac(cadr(args), env), ac_if(cddr(args), env)]);
    }
  }
};
ac_dbname33 = function (name, env) {
  if (ac_symbol63(name)) {
    return(cons([name], env));
  } else {
    return(env);
  }
};
ac_dbname = function (env) {
  if (null63(env)) {
    return(false);
  } else {
    if (pair63(car(env))) {
      return(caar(env));
    } else {
      return(ac_dbname(cdr(env)));
    }
  }
};
ar_false63 = function (x) {
  return(x === "nil" || x === undefined || x === [] || ! atom63(x) && none63(x));
};
ac_denil = function (x) {
  if (! atom63(x)) {
    return(cons(ac_denil_car(car(x)), ac_denil_cdr(cdr(x))));
  } else {
    return(x);
  }
};
ac_denil_car = function (x) {
  if (x === "nil") {
    return("nil");
  } else {
    return(ac_denil(x));
  }
};
ac_denil_cdr = function (x) {
  if (x === "nil") {
    return([]);
  } else {
    return(ac_denil(x));
  }
};
ac_niltree = function (x) {
  if (! atom63(x)) {
    if (none63(x)) {
      return("nil");
    } else {
      return(cons(ac_niltree(car(x)), ac_niltree(cdr(x))));
    }
  } else {
    if (! x || x === "nil" || x === []) {
      return("nil");
    } else {
      return(x);
    }
  }
};
ac_lex63 = function (x, env) {
  return(in63(x, env));
};
ac_namespace = unique("_");
ac_global_name = function (x) {
  return(ac_namespace + x);
};
ac_var_ref = function (x, env) {
  if (ac_lex63(x, env)) {
    return(x);
  } else {
    return(ac_global_name(x));
  }
};
setenv("xdef", {_stash: true, macro: function (a, b) {
  return(["set", "__" + a, b]);
}});
__car = car;
__cdr = cdr;
__cons = cons;
__t = "t";
__nil = "nil";
__list = function () {
  var lst = unstash(Array.prototype.slice.call(arguments, 0));
  return(lst);
};
arc_list63 = function (x) {
  return(pair63(x) || x === "nil" || x === []);
};
__43 = function () {
  var args = unstash(Array.prototype.slice.call(arguments, 0));
  if (null63(args)) {
    return(0);
  } else {
    if (string63(car(args))) {
      return(apply(cat, map(function (x) {
        return(ar_coerce(x, "string"));
      }, args)));
    } else {
      if (arc_list63(car(args))) {
        return(ac_niltree(apply(join, map(ar_nil_terminate, args))));
      } else {
        return(apply(_43, args));
      }
    }
  }
};
___ = _;
__47 = _47;
__42 = _42;
ar_622 = function (x, y) {
  return(tnil(x > y));
};
__62 = function () {
  var args = unstash(Array.prototype.slice.call(arguments, 0));
  return(pairwise(ar_622, args));
};
ar_602 = function (x, y) {
  return(tnil(x < y));
};
__60 = function () {
  var args = unstash(Array.prototype.slice.call(arguments, 0));
  return(pairwise(ar_602, args));
};
__len = function (x) {
  if (string63(x)) {
    return(_35(x));
  } else {
    return(_35(ar_nil_terminate(x)));
  }
};
vector_type = unique("vec");
vector = function () {
  var xs = unstash(Array.prototype.slice.call(arguments, 0));
  return(join([vector_type], xs));
};
vector63 = function (x) {
  return(pair63(x) && car(x) === vector_type);
};
vector_ref = function (x, i) {
  if (! vector63(x)) {
    throw new Error("vector-ref: expected vector, got " + string(x));
  }
  return(x[i + 1]);
};
ar_tagged63 = function (x) {
  return(vector63(x) && vector_ref(x, 0) === "tagged");
};
ar_tag = function (type, rep) {
  if (ar_type(rep) === type) {
    return(rep);
  } else {
    return(vector("tagged", type, rep));
  }
};
__annotate = ar_tag;
ar_type = function (x) {
  if (ar_tagged63(x)) {
    return(vector_ref(x, 1));
  } else {
    if (pair63(x)) {
      return("cons");
    } else {
      if (ac_string63(x)) {
        return("string");
      } else {
        if (ac_symbol63(x)) {
          return("sym");
        } else {
          if (null63(x)) {
            return("sym");
          } else {
            if (function63(x)) {
              return("fn");
            } else {
              if (number63(x)) {
                return("num");
              } else {
                throw new Error("Type: unknown type " + string(x));
              }
            }
          }
        }
      }
    }
  }
};
__type = ar_type;
tnil = function (x) {
  if (x) {
    return("t");
  } else {
    return("nil");
  }
};
pairwise = function (pred, lst) {
  if (null63(lst)) {
    return("t");
  } else {
    if (null63(cdr(lst))) {
      return("t");
    } else {
      if (!( pred(car(lst), cadr(lst)) === "nil")) {
        return(pairwise(pred, cdr(lst)));
      } else {
        return("nil");
      }
    }
  }
};
ar_is2 = function (a, b) {
  return(tnil(a === b || string63(a) && string63(b) && a === b || ar_false63(a) && ar_false63(b)));
};
__is = function () {
  var args = unstash(Array.prototype.slice.call(arguments, 0));
  return(pairwise(ar_is2, args));
};
ac_set = function (x, env) {
  return(join(["do"], ac_setn(x, env)));
};
ac_setn = function (x, env) {
  if (null63(x)) {
    return([]);
  } else {
    return(cons(ac_set1(ac_macex(car(x)), cadr(x), env), ac_setn(cddr(x), env)));
  }
};
ac_set1 = function (a, b1, env) {
  if (ac_symbol63(a)) {
    var b = ac(b1, ac_dbname33(a, env));
    var _e;
    if (a === "nil") {
      throw new Error("Can't rebind nil");
      _e = undefined;
    } else {
      var _e1;
      if (a === "t") {
        throw new Error("Can't rebind t");
        _e1 = undefined;
      } else {
        var _e2;
        if (ac_lex63(a, env)) {
          _e2 = ["set", a, "zz"];
        } else {
          _e2 = ["set", ac_global_name(a), "zz"];
        }
        _e1 = _e2;
      }
      _e = _e1;
    }
    return(["let", "zz", b, _e, "zz"]);
  } else {
    return(err("First arg to set must be a symbol", a));
  }
};
ac_body = function (body, env) {
  return(map(function (x) {
    return(ac(x, env));
  }, body));
};
ac_body42 = function (body, env) {
  if (null63(body)) {
    return([["quote", "nil"]]);
  } else {
    return(ac_body(body, env));
  }
};
ac_fn = function (args, body, env) {
  var a = ac_denil(args);
  var _e3;
  if (a === "nil") {
    _e3 = [];
  } else {
    _e3 = a;
  }
  return(join(["fn", _e3], ac_body42(body, join(ac_arglist(args), env))));
};
ac_arglist = function (a) {
  if (null63(a)) {
    return([]);
  } else {
    if (ac_symbol63(a)) {
      return([a]);
    } else {
      if (ac_symbol63(cdr(a))) {
        return([car(a), cdr(a)]);
      } else {
        return(cons(car(a), ac_arglist(cdr(a))));
      }
    }
  }
};
ac_call = function (f, args, env) {
  if (xcar(f) === "fn") {
    return(join([ac(f, env)], ac_args(cadr(f), args, env)));
  } else {
    return(["ar-apply", ac(f, env), join(["list"], map(function (x) {
      return(ac(x, env));
    }, args))]);
  }
};
ac_macro63 = function (f) {
  return(false);
};
ac_macex = function (e, once) {
  if (pair63(e)) {
    var m = ac_macro63(car(e));
    if (m) {
      var expansion = ac_denil(apply(m, map(ac_niltree, cdr(e))));
      if (null63(once)) {
        return(ac_macex(expansion));
      } else {
        return(expansion);
      }
    } else {
      return(e);
    }
  } else {
    return(e);
  }
};
ac_args = function (names, exprs, env) {
  if (null63(exprs)) {
    return([]);
  } else {
    var _e4;
    if (pair63(names)) {
      _e4 = car(names);
    }
    var _e5;
    if (pair63(names)) {
      _e5 = cdr(names);
    } else {
      _e5 = [];
    }
    return(cons(ac(car(exprs), ac_dbname33(_e4, env)), ac_args(_e5, cdr(exprs), env)));
  }
};
ar_apply = function (f, args) {
  if (function63(f)) {
    return(apply(f, args));
  } else {
    if (! atom63(f)) {
      return(f[car(args)]);
    } else {
      if (string63(f)) {
        return(char(f, car(args)));
      } else {
        throw new Error("ar-apply: bad " + string(f) + " " + string(args));
      }
    }
  }
};
__apply = function (f) {
  var _r54 = unstash(Array.prototype.slice.call(arguments, 1));
  var _id = _r54;
  var args = cut(_id, 0);
  return(ar_apply(f, ar_apply_args(args)));
};
ar_nil_terminate = function (l) {
  if (null63(l) || l === [] || l === "nil") {
    return([]);
  } else {
    return(cons(car(l), ar_nil_terminate(cdr(l))));
  }
};
ar_apply_args = function (args) {
  if (null63(args)) {
    return([]);
  } else {
    if (null63(cdr(args))) {
      return(ar_nil_terminate(car(args)));
    } else {
      return(cons(car(args), ar_apply_args(cdr(args))));
    }
  }
};
var delimiters = {"(": true, ")": true, "\n": true, ";": true};
var whitespace = {" ": true, "\n": true, "\t": true};
var peek_char = function (s) {
  var _id1 = s;
  var pos = _id1.pos;
  var len = _id1.len;
  var string = _id1.string;
  if (pos < len) {
    return(char(string, pos));
  }
};
var read_char = function (s) {
  var c = peek_char(s);
  if (c) {
    s.pos = s.pos + 1;
    return(c);
  }
};
var skip_non_code = function (s) {
  while (true) {
    var c = peek_char(s);
    if (nil63(c)) {
      break;
    } else {
      if (whitespace[c]) {
        read_char(s);
      } else {
        if (c === ";") {
          while (c && !( c === "\n")) {
            c = read_char(s);
          }
          skip_non_code(s);
        } else {
          break;
        }
      }
    }
  }
};
var literals = {"-nan": 0 / 0, "false": false, nan: 0 / 0, "true": true, "-inf": -1 / 0, inf: 1 / 0};
var ac_read_atom = function (s) {
  var str = "";
  while (true) {
    var c = peek_char(s);
    if (c && (! whitespace[c] && ! delimiters[c])) {
      str = str + read_char(s);
    } else {
      break;
    }
  }
  var x = literals[str];
  if (is63(x)) {
    return(x);
  } else {
    var n = number(str);
    if (!( nil63(n) || nan63(n) || inf63(n))) {
      return(n);
    } else {
      return(str);
    }
  }
};
var _f = reader["read-table"]["\""];
var ac_read_string = function (s) {
  var str = _f(s);
  if (! str) {
    return("");
  } else {
    return(escape(str));
  }
};
arc_read = function (s) {
  var old_atom = reader["read-table"][""];
  var old_str = reader["read-table"]["\""];
  reader["read-table"][""] = ac_read_atom;
  reader["read-table"]["\""] = ac_read_string;
  var r = reader["read-all"](reader.stream(s));
  reader["read-table"][""] = old_atom;
  reader["read-table"]["\""] = old_str;
  return(r);
};
arc_eval = function (expr) {
  return(eval(ac(expr, [])));
};
__eval = arc_eval;
setenv("arc", {_stash: true, macro: function () {
  var exprs = unstash(Array.prototype.slice.call(arguments, 0));
  return(["last", join(["quote"], map(function (e) {
    if (id_literal63(e)) {
      return(map(arc_eval, arc_read(inner(e))));
    } else {
      return(arc_eval(e));
    }
  }, exprs))]);
}});
