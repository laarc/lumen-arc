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
              throw new Error("Bad object in expression " + string(x));
            }
          }
        }
      }
    }
  }
};
ac_string63 = function (x) {
  return(string_literal63(x));
};
ac_symbol63 = function (x) {
  return(string63(x));
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
null63 = function (x) {
  return(! is63(x) || ! atom63(x) && none63(x));
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
var ac_lex63 = function (x, env) {
  return(in63(x, env));
};
var _ns = unique("_");
var ac_global_name = function (x) {
  return(_ns + x);
};
ac_var_ref = function (x, env) {
  if (ac_lex63(x, env)) {
    return(x);
  } else {
    return(ac_global_name(x));
  }
};
var delimiters = {"(": true, ")": true, "\n": true, ";": true};
var whitespace = {" ": true, "\n": true, "\t": true};
var peek_char = function (s) {
  var _id = s;
  var pos = _id.pos;
  var len = _id.len;
  var string = _id.string;
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
var read_atom = function (s) {
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
arc_read = function (s) {
  var old = reader["read-table"][""];
  reader["read-table"][""] = read_atom;
  var r = reader["read-string"](s);
  reader["read-table"][""] = old;
  return(r);
};
arc_eval = function (expr) {
  return(eval(ac(expr, [])));
};
