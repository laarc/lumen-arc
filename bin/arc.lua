reader = require("reader")
compiler = require("compiler")
function ac(x, env)
  env = env or {}
  if ac_string63(x) then
    return(ac_string(x, env))
  else
    if ac_literal63(x) then
      return(x)
    else
      if x == "nil" then
        return({"quote", "nil"})
      else
        if ac_symbol63(x) then
          return(ac_var_ref(x, env))
        else
          if xcar(x) == "quote" then
            return({"quote", ac_niltree(cadr(x))})
          else
            if xcar(x) == "if" then
              return(ac_if(cdr(x), env))
            else
              if xcar(x) == "fn" then
                return(ac_fn(cadr(x), cddr(x), env))
              else
                if xcar(x) == "assign" then
                  return(ac_set(cdr(x), env))
                else
                  if not atom63(x) then
                    return(ac_call(car(x), cdr(x), env))
                  else
                    error("Bad object in expression " .. string(x))
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end
function ac_string63(x)
  return(string_literal63(x))
end
function ac_symbol63(x)
  return(string63(x))
end
function ac_string(x, env)
  return(x)
end
function ac_literal63(x)
  return(boolean63(x) or ac_string63(x) or number63(x) or not atom63(x) and none63(x))
end
function xcar(x)
  if not atom63(x) then
    return(hd(x))
  end
end
function xcdr(x)
  if not atom63(x) then
    return(tl(x))
  end
end
dot = unique("dot")
function car(x)
  if not x then
    return(nil)
  else
    if atom63(x) then
      error("car: expected list, got " .. string(x))
    else
      if none63(x) then
        return(nil)
      else
        local v = hd(x)
        if v == dot then
          error("car: bad cons " .. string(x))
        else
          return(v)
        end
      end
    end
  end
end
function cdr(x)
  if not x then
    return(nil)
  else
    if atom63(x) then
      error("cdr: expected list, got " .. string(x))
    else
      if none63(x) then
        return(nil)
      else
        local v = tl(x)
        if xcar(v) == dot then
          if not one63(tl(v)) then
            error("cdr: bad cons " .. string(x))
          else
            return(hd(tl(v)))
          end
        else
          return(v)
        end
      end
    end
  end
end
function cadr(x)
  return(car(cdr(x)))
end
function cddr(x)
  return(cdr(cdr(x)))
end
function cons(x, y)
  if atom63(y) then
    if y then
      return({x, dot, y})
    else
      return({x})
    end
  else
    return(join({x}, y))
  end
end
function null63(x)
  return(not is63(x) or not atom63(x) and none63(x))
end
function pair63(x)
  return(not atom63(x))
end
function ac_if(args, env)
  if null63(args) then
    return({"quote", "nil"})
  else
    if null63(cdr(args)) then
      return(ac(car(args), env))
    else
      return({"if", {"not", {"ar-false?", ac(car(args), env)}}, ac(cadr(args), env), ac_if(cddr(args), env)})
    end
  end
end
function ac_dbname33(name, env)
  if ac_symbol63(name) then
    return(cons({name}, env))
  else
    return(env)
  end
end
function ac_dbname(env)
  if null63(env) then
    return(false)
  else
    if pair63(car(env)) then
      return(caar(env))
    else
      return(ac_dbname(cdr(env)))
    end
  end
end
function ar_false63(x)
  return(x == "nil" or x == nil or x == {} or not atom63(x) and none63(x))
end
function ac_denil(x)
  if not atom63(x) then
    return(cons(ac_denil_car(car(x)), ac_denil_cdr(cdr(x))))
  else
    return(x)
  end
end
function ac_denil_car(x)
  if x == "nil" then
    return("nil")
  else
    return(ac_denil(x))
  end
end
function ac_denil_cdr(x)
  if x == "nil" then
    return({})
  else
    return(ac_denil(x))
  end
end
function ac_niltree(x)
  if not atom63(x) then
    if none63(x) then
      return("nil")
    else
      return(cons(ac_niltree(car(x)), ac_niltree(cdr(x))))
    end
  else
    if not x or x == "nil" or x == {} then
      return("nil")
    else
      return(x)
    end
  end
end
function ac_lex63(x, env)
  return(in63(x, env))
end
ac_namespace = unique("_")
function ac_global_name(x)
  return(ac_namespace .. x)
end
function ac_var_ref(x, env)
  if ac_lex63(x, env) then
    return(x)
  else
    return(ac_global_name(x))
  end
end
setenv("xdef", {_stash = true, macro = function (a, b)
  return({"set", "__" .. a, b})
end})
__car = car
__cdr = cdr
__cons = cons
__t = "t"
__nil = "nil"
__list = function (...)
  local lst = unstash({...})
  return(lst)
end
function arc_list63(x)
  return(pair63(x) or x == "nil" or x == {})
end
__43 = function (...)
  local args = unstash({...})
  if null63(args) then
    return(0)
  else
    if string63(car(args)) then
      return(apply(cat, map(function (x)
        return(ar_coerce(x, "string"))
      end, args)))
    else
      if arc_list63(car(args)) then
        return(ac_niltree(apply(join, map(ar_nil_terminate, args))))
      else
        return(apply(_43, args))
      end
    end
  end
end
___ = _
__47 = _47
__42 = _42
function tnil(x)
  if x then
    return("t")
  else
    return("nil")
  end
end
function pairwise(pred, lst)
  if null63(lst) then
    return("t")
  else
    if null63(cdr(lst)) then
      return("t")
    else
      if not( pred(car(lst), cadr(lst)) == "nil") then
        return(pairwise(pred, cdr(lst)))
      else
        return("nil")
      end
    end
  end
end
function ar_is2(a, b)
  return(tnil(a == b or string63(a) and string63(b) and a == b or ar_false63(a) and ar_false63(b)))
end
__is = function (...)
  local args = unstash({...})
  return(pairwise(ar_is2, args))
end
function ac_set(x, env)
  return(join({"do"}, ac_setn(x, env)))
end
function ac_setn(x, env)
  if null63(x) then
    return({})
  else
    return(cons(ac_set1(ac_macex(car(x)), cadr(x), env), ac_setn(cddr(x), env)))
  end
end
function ac_set1(a, b1, env)
  if ac_symbol63(a) then
    local b = ac(b1, ac_dbname33(a, env))
    local _e
    if a == "nil" then
      error("Can't rebind nil")
      _e = nil
    else
      local _e1
      if a == "t" then
        error("Can't rebind t")
        _e1 = nil
      else
        local _e2
        if ac_lex63(a, env) then
          _e2 = {"set", a, "zz"}
        else
          _e2 = {"set", ac_global_name(a), "zz"}
        end
        _e1 = _e2
      end
      _e = _e1
    end
    return({"let", "zz", b, _e, "zz"})
  else
    return(err("First arg to set must be a symbol", a))
  end
end
function ac_body(body, env)
  return(map(function (x)
    return(ac(x, env))
  end, body))
end
function ac_body42(body, env)
  if null63(body) then
    return({{"quote", "nil"}})
  else
    return(ac_body(body, env))
  end
end
function ac_fn(args, body, env)
  local a = ac_denil(args)
  local _e3
  if a == "nil" then
    _e3 = {}
  else
    _e3 = a
  end
  return(join({"fn", _e3}, ac_body42(body, join(ac_arglist(args), env))))
end
function ac_arglist(a)
  if null63(a) then
    return({})
  else
    if ac_symbol63(a) then
      return({a})
    else
      if ac_symbol63(cdr(a)) then
        return({car(a), cdr(a)})
      else
        return(cons(car(a), ac_arglist(cdr(a))))
      end
    end
  end
end
function ac_call(f, args, env)
  if xcar(f) == "fn" then
    return(join({ac(f, env)}, ac_args(cadr(f), args, env)))
  else
    return({"ar-apply", ac(f, env), join({"list"}, map(function (x)
      return(ac(x, env))
    end, args))})
  end
end
function ac_macro63(f)
  return(false)
end
function ac_macex(e, once)
  if pair63(e) then
    local m = ac_macro63(car(e))
    if m then
      local expansion = ac_denil(apply(m, map(ac_niltree, cdr(e))))
      if null63(once) then
        return(ac_macex(expansion))
      else
        return(expansion)
      end
    else
      return(e)
    end
  else
    return(e)
  end
end
function ac_args(names, exprs, env)
  if null63(exprs) then
    return({})
  else
    local _e4
    if pair63(names) then
      _e4 = car(names)
    end
    local _e5
    if pair63(names) then
      _e5 = cdr(names)
    else
      _e5 = {}
    end
    return(cons(ac(car(exprs), ac_dbname33(_e4, env)), ac_args(_e5, cdr(exprs), env)))
  end
end
function ar_apply(f, args)
  if function63(f) then
    return(apply(f, args))
  else
    if not atom63(f) then
      return(f[car(args) + 1])
    else
      if string63(f) then
        return(char(f, car(args)))
      else
        error("ar-apply: bad " .. string(f) .. " " .. string(args))
      end
    end
  end
end
function ar_nil_terminate(l)
  if null63(l) or l == {} or l == "nil" then
    return({})
  else
    return(cons(car(l), ar_nil_terminate(cdr(l))))
  end
end
local delimiters = {["("] = true, [")"] = true, ["\n"] = true, [";"] = true}
local whitespace = {[" "] = true, ["\n"] = true, ["\t"] = true}
local function peek_char(s)
  local _id = s
  local pos = _id.pos
  local len = _id.len
  local string = _id.string
  if pos < len then
    return(char(string, pos))
  end
end
local function read_char(s)
  local c = peek_char(s)
  if c then
    s.pos = s.pos + 1
    return(c)
  end
end
local function skip_non_code(s)
  while true do
    local c = peek_char(s)
    if nil63(c) then
      break
    else
      if whitespace[c] then
        read_char(s)
      else
        if c == ";" then
          while c and not( c == "\n") do
            c = read_char(s)
          end
          skip_non_code(s)
        else
          break
        end
      end
    end
  end
end
local literals = {["-nan"] = 0 / 0, ["false"] = false, nan = 0 / 0, ["true"] = true, ["-inf"] = -1 / 0, inf = 1 / 0}
local function read_atom(s)
  local str = ""
  while true do
    local c = peek_char(s)
    if c and (not whitespace[c] and not delimiters[c]) then
      str = str .. read_char(s)
    else
      break
    end
  end
  local x = literals[str]
  if is63(x) then
    return(x)
  else
    local n = number(str)
    if not( nil63(n) or nan63(n) or inf63(n)) then
      return(n)
    else
      return(str)
    end
  end
end
function arc_read(s)
  local old = reader["read-table"][""]
  reader["read-table"][""] = read_atom
  local r = reader["read-string"](s)
  reader["read-table"][""] = old
  return(r)
end
function arc_eval(expr)
  return(eval(ac(expr, {})))
end
