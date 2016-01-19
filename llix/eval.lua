local insert, remove
do
  local _obj_0 = table
  insert, remove = _obj_0.insert, _obj_0.remove
end
local _insert = insert
insert = function(t, i, o)
  do
    local _with_0 = t
    if not (o) then
      _insert(t, i)
    else
      _insert(t, i, o)
    end
    return _with_0
  end
end
local inspect = require("inspect")
local iprint
iprint = function(...)
  return print((inspect({
    ...
  })):match("^{ (.*) }$"))
end
local type_s
type_s = function(obj)
  return type(obj) == "string"
end
local type_t
type_t = function(obj)
  return type(obj) == "table"
end
local is_funcall
is_funcall = function(t)
  return type_t(t) and t.label == "funcall"
end
local noop
noop = function(self)
  return self
end
local cp_tbl
cp_tbl = function(t)
  local _tbl_0 = { }
  for k, v in pairs(t) do
    _tbl_0[k] = (type_t(v)) and (cp_tbl(v)) or v
  end
  return _tbl_0
end
local strmguard
strmguard = function(str, t)
  for i = 1, #t, 2 do
    if not (type_s(t[i]) or type(t[i + 1]) == "function") then
      error("strmguard failed")
    end
    if str:match(t[i]) then
      return t[i + 1]()
    end
  end
  if t.default then
    return t.default()
  end
end
local strip_string
strip_string = function(str)
  do
    local dq = str:match("^\"(.*)\"")
    if not (dq) then
      dq = str:gsub("^%[(=*)%[(.*)%]%1%]", "%2")
    end
    return dq
  end
end
local deeval_str
deeval_str = function(x)
  if type_s(x) then
    return noop(x:gsub("^(.*)", "\"%1\""))
  else
    return x or "nil"
  end
end
local binop_table = {
  "<=",
  ">=",
  "~=",
  "==",
  "<",
  ">",
  "..",
  "+",
  "-",
  "*",
  "/",
  "%",
  "^",
  ">>",
  "<<",
  "|",
  "&"
}
local binop = setmetatable((function()
  local _tbl_0 = { }
  for _index_0 = 1, #binop_table do
    local op = binop_table[_index_0]
    _tbl_0[op] = (load("return function(l, r) return l " .. tostring(op) .. " r end"))()
  end
  return _tbl_0
end)(), {
  __index = {
    ["//"] = function(l, r)
      if r == 0 then
        return error("attempt to divide by zero")
      else
        return l // r
      end
    end
  },
  __call = function(self, op, left, right)
    return self[op](left, right)
  end
})
local uniop_table = {
  "-",
  "~",
  "#",
  "not"
}
local uniop = setmetatable((function()
  local _tbl_0 = { }
  for _index_0 = 1, #uniop_table do
    local op = uniop_table[_index_0]
    _tbl_0[op] = (load("return function(e) return " .. tostring(op) .. "(e) end"))()
  end
  return _tbl_0
end)(), {
  __index = function(op)
    return (load("return function(e) return " .. tostring(op) .. "(e) end"))()
  end,
  __call = function(self, op, e)
    return self[op](e)
  end
})
local funstack = {
  pushcresume = function(self, fun)
    return coroutine.resume((insert(self, {
      coroutine.create(fun)
    }))[#self][1])
  end,
  stopregret = function(self, ret)
    return coroutine.yield((insert(self[#self], ret))[#self][1])
  end,
  pop = function(self)
    return remove(self)
  end
}
local __ENV = {
  _VERSION = _VERSION,
  arg = cp_tbl(arg),
  assert = assert,
  bit32 = cp_tbl(bit32),
  collectgarbage = collectgarbage,
  coroutine = cp_tbl(coroutine),
  debug = cp_tbl(debug),
  dofile = dofile,
  error = error,
  getmetatable = getmetatable,
  io = cp_tbl(io),
  ipairs = ipairs,
  load = load,
  loadfile = loadfile,
  loadstring = loadstring,
  math = cp_tbl(math),
  module = module,
  next = next,
  os = cp_tbl(os),
  package = package,
  print = print,
  pairs = pairs,
  pcall = pcall,
  rawequal = rawequal,
  rawget = rawget,
  rawset = rawset,
  require = require,
  select = select,
  setmetatable = setmetatable,
  string = cp_tbl(string),
  table = cp_tbl(table),
  tonumber = tonumber,
  tostring = tostring,
  type = type,
  unpack = unpack,
  utf8 = cp_tbl(utf8),
  xpcall = xpcall
}
do
  __ENV._G = __ENV
  __ENV._ENV = __ENV
  __ENV.package.loaded = __ENV
end
local expand_tbl, eval_tbl, eval_exp, eval_funcdef, eval_args, eval_funcall, eval_return, eval_varlist, eval_body, kinit, eval
expand_tbl = function(tbl, is_dec, env, k0, k)
  if type_t(tbl[2]) then
    local _exp_0 = tbl[2].label
    if nil == _exp_0 then
      env._llix_tmp_tbl = env._llix_tmp_tbl and env._llix_tmp_tbl[tbl[2][1]] or env[tbl[1]][tbl[2][1]]
      return expand_tbl(tbl[2], is_dec, env, k0, k)
    elseif "exp" == _exp_0 then
      return eval_exp(tbl[2], env, k0, function(e)
        tbl[2] = e
        return expand_tbl(tbl, is_dec, env, k0, k)
      end)
    elseif "funcall" == _exp_0 then
      return eval_funcall(tbl[2][1], tbl[2][2], env, k0, function(e)
        tbl[2] = remove(e, 1)
        return expand_tbl(tbl, is_dec, env, k0, k)
      end)
    end
  else
    local f
    f = function(e)
      do
        local tmpt = env._llix_tmp_tbl
        if tmpt then
          return tmpt[e]
        else
          do
            local p = env[tbl[1]]
            if p then
              return p[e]
            else
              return error("attempt to index a nil valie (local '" .. tostring(tbl[1]) .. "')")
            end
          end
        end
      end
    end
    if not (is_dec) then
      if type(tbl[2]) == "number" then
        return k(f(tbl[2]))
      else
        do
          local m = tbl[2]:match("^\"(.*)\"$")
          if m then
            return k(f(m))
          else
            do
              m = tonumber(tbl[2])
              if m then
                return k(f(m))
              else
                return eval_exp(tbl[2], env, k0, function(e)
                  return k(f(e))
                end)
              end
            end
          end
        end
      end
    else
      return k((env._llix_tmp_tbl and env._llix_tmp_tbl or env[tbl[1]]), tbl[2])
    end
  end
end
eval_tbl = function(fields, pos, env, k0, k)
  if pos == nil then
    pos = 1
  end
  if not (fields[1]) then
    return k(fields)
  else
    local key, val
    local head = remove(fields, 1)
    local tblf
    tblf = function(val, pos)
      return function(key)
        return eval_exp(val, env, k0, function(x)
          return eval_tbl(fields, pos, env, k0, function(y)
            return k((function()
              do
                y[key] = x
                return y
              end
            end)())
          end)
        end)
      end
    end
    local _exp_0 = type(head)
    if "table" == _exp_0 then
      local _exp_1 = head.label
      if not fields[1] and "funcall" == _exp_1 then
        return eval_funcall(head[1], head[2], env, k0, function(t)
          return k(t)
        end)
      elseif "tableaccess" == _exp_1 or "constructor" == _exp_1 then
        key = pos
        val = (head.label == "tableaccess" and head or {
          label = "constructor"
        })
        pos = pos + 1
      elseif nil == _exp_1 then
        key = type_t(head[1]) and remove(head[1]) or deeval_str(head[1])
        val = head[2]
      end
      return eval_exp(key, env, k0, (tblf(val, pos)))
    elseif "string" == _exp_0 then
      key = pos
      val = head
      pos = pos + 1
      if val == "..." and not fields[1] then
        return eval_exp(key, env, k0, function(key)
          return eval_args(env[head], env, k0, function(vars)
            return eval_tbl(fields, pos, env, k0, function(y)
              return k((function()
                do
                  for i = 1, #vars do
                    y[i + key] = vars[i]
                  end
                  return y
                end
              end)())
            end)
          end)
        end)
      else
        return (tblf(val, pos))(key)
      end
    end
  end
end
eval_exp = function(exp, env, k0, k)
  local _exp_0 = type(exp)
  if "string" == _exp_0 then
    return k(strmguard(exp, {
      "^%d",
      function()
        return tonumber(exp)
      end,
      "^%.%d",
      function()
        return tonumber(exp)
      end,
      "^nil$",
      function()
        return nil
      end,
      "^true$",
      function()
        return true
      end,
      "^false$",
      function()
        return false
      end,
      "^[_a-zA-Z]",
      function()
        return env[exp] or nil
      end,
      "^%.%.%.$",
      function()
        return eval_args(env[exp][1], env, k0, function(e)
          return k(remove(e, 1))
        end)
      end,
      default = function()
        return strip_string(exp)
      end
    }))
  elseif "table" == _exp_0 then
    local _exp_1 = exp.label
    if "constructor" == _exp_1 then
      exp.label = nil
      return eval_tbl(exp, _, env, k0, k)
    elseif "funcall" == _exp_1 then
      return eval_funcall(exp[1], exp[2], env, k0, function(t)
        return k((remove(t, 1)))
      end)
    elseif "annonymousfuncdef" == _exp_1 then
      return eval_funcdef(exp, env, k0, k)
    elseif "tableaccess" == _exp_1 then
      return k(({
        expand_tbl(exp, _, env, k0, noop)
      })[1])
    elseif "exp" == _exp_1 then
      local op
      op = exp.op
      if exp[2] then
        local _exp_2 = exp.op
        if "or" == _exp_2 then
          return eval_exp(exp[1], env, k0, function(do_l)
            if do_l then
              return k(do_l)
            else
              return eval_exp(exp[2], env, k0, k)
            end
          end)
        elseif "and" == _exp_2 then
          return eval_exp(exp[1], env, k0, function(do_l)
            if do_l then
              return eval_exp(exp[2], env, k0, k)
            else
              return k(do_l)
            end
          end)
        else
          return eval_exp(exp[1], env, k0, function(left)
            return eval_exp(exp[2], env, k0, function(right)
              return k(binop(op, left, right))
            end)
          end)
        end
      else
        return eval_exp(exp[1], env, k0, function(exp)
          return k(uniop(op, exp))
        end)
      end
    else
      return k(exp)
    end
  else
    return k(exp)
  end
end
eval_funcdef = function(def, env, k0, k)
  local nenv = cp_tbl(env)
  def.name = def.name or ""
  nenv[def.name] = function(...)
    local gargs = {
      ...
    }
    do
      local args = def.args
      if args then
        for i = 1, #args do
          if not (args[i] == "...") then
            nenv[args[i]] = remove(gargs, 1)
          else
            nenv[args[i]] = cp_tbl(gargs)
            break
          end
        end
      end
    end
    funstack:pushcresume(function()
      return eval(def.body, nenv, k0)
    end)
    local ret = funstack:pop()
    return ret[2] and unpack(ret[2] or nil)
  end
  return k(nenv[def.name])
end
eval_args = function(arglist, env, k0, k)
  if not (arglist[1]) then
    return k(arglist)
  else
    local head = remove(arglist, 1)
    local argf
    argf = function()
      return eval_exp(head, env, k0, function(x)
        return eval_args(arglist, env, k0, function(y)
          _insert(y, 1, x)
          return k(y)
        end)
      end)
    end
    if not (arglist[1]) then
      if is_funcall(head) then
        return eval_funcall(head[1], head[2], env, k0, function(x)
          return eval_args(arglist, env, k0, function(y)
            return k((function()
              do
                for i = 1, #x do
                  _insert(y, x[i])
                end
                return y
              end
            end)())
          end)
        end)
      elseif head == "..." then
        return eval_args(env[head], env, k0, function(t)
          return k(t)
        end)
      else
        return argf()
      end
    else
      return argf()
    end
  end
end
eval_funcall = function(func, args, env, k0, k)
  if func == "throw" then
    return k0.excep(args[1], env)
  end
  local run
  run = function(args)
    return function(func)
      return eval_args(args, env, k0, function(fmt_args)
        return k({
          func(unpack(fmt_args))
        })
      end)
    end
  end
  if type_t(func) then
    local _exp_0 = func.label
    if "tableaccess" == _exp_0 then
      return expand_tbl(func, _, env, k0, function(func)
        return (function(args)
          return (run(args))((env[func] and env[func] or func))
        end)((function()
          do
            if args.label == "colonfunc" then
              local s = func
              func = func[args.func]
              args = args.args or { }
              _insert(args, 1, s)
            end
            return args
          end
        end)())
      end)
    elseif "annonymousfuncdef" == _exp_0 then
      return eval_funcdef(func, env, k0, run(args))
    elseif "funcall" == _exp_0 then
      return eval_exp(func, env, k0, function(f)
        return (run(args))(f)
      end)
    end
  end
  return (function(args)
    return (run(args))((env[func] and env[func] or func))
  end)((function()
    do
      if args.label == "colonfunc" then
        local s = func
        if (type_s(func)) and (func:match("^\"") or func:match("^%[=*%[")) then
          func = string[args.func]
        else
          func = env[func][args.func]
        end
        args = args.args or { }
        _insert(args, 1, s)
      end
      return args
    end
  end)())
end
eval_return = function(body, ret, env, k0, k)
  if ret == nil then
    ret = { }
  end
  if not (body[1]) then
    if funstack[1] then
      return k(funstack:stopregret(ret))
    else
      return k(ret)
    end
  else
    body = cp_tbl(body)
    local head = remove(body, 1)
    if not body[1] and is_funcall(head) then
      return eval_funcall(head[1], head[2], env, k0, function(t)
        for _index_0 = 1, #t do
          local i = t[_index_0]
          _insert(ret, i)
        end
        return eval_return(body, ret, env, k0, k)
      end)
    else
      return eval_exp(head, env, k0, function(x)
        return eval_return(body, insert(ret, x), env, k0, k)
      end)
    end
  end
end
eval_varlist = function(left, right, regtbl, env, k0, k)
  if right == nil then
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _ = 1, #left do
        _accum_0[_len_0] = ""
        _len_0 = _len_0 + 1
      end
      right = _accum_0
    end
  end
  local f
  f = function(t)
    for i = 1, #left do
      if rawget(env, left[i]) then
        env[left[i]] = t[i]
      else
        regtbl[left[i]] = t[i]
      end
    end
  end
  return k((function()
    if not right[2] and is_funcall(regtbl[1]) then
      return eval_funcall(right[1][1], right[1][2], env, k0, f)
    else
      return eval_args(right, env, k0, f)
    end
  end)())
end
eval_body = function(body, env, k0, k)
  local _exp_0 = body.label
  if "try" == _exp_0 then
    local k1 = {
      loop = k0,
      excep = function(args, env)
        return eval_exp(args, env, k0, function(t)
          env._ERR = t
          return eval(body.catchbody, env, k0, noop)
        end)
      end
    }
    return k(eval(body.body, env, k1, function(e)
      for k, v in pairs(e) do
        if env[k] then
          env[k] = v
        end
      end
    end))
  elseif "do" == _exp_0 then
    return k(eval(body, (cp_tbl(env)), k0, function(e)
      for k, v in pairs(e) do
        if env[k] then
          env[k] = v
        end
      end
    end))
  elseif "return" == _exp_0 then
    return eval_return(body, _, env, k0, k)
  elseif "break" == _exp_0 then
    return k0.loop()
  elseif "varlist" == _exp_0 then
    return eval_varlist(body[1], body[2], __ENV, env, k0, k)
  elseif "localvarlist" == _exp_0 then
    return eval_varlist(body[1], _, env, env, k0, function()
      return eval_varlist(body[1], body[2], env, env, k0, k)
    end)
  elseif "funcall" == _exp_0 then
    return eval_funcall(body[1], body[2], env, k0, k)
  elseif "tableaccess" == _exp_0 then
    return expand_tbl(body, _, env, k0, noop)
  elseif "funcdef" == _exp_0 then
    return k(eval_funcdef(body, env, k0, function(f)
      __ENV[body.name] = f
    end))
  elseif "localfuncdef" == _exp_0 then
    return k(eval_funcdef(body, env, k0, function(f)
      env[body.name] = f
    end))
  elseif "while" == _exp_0 then
    local whilef
    whilef = function(env)
      return eval_exp(body.cond, env, k0, function(cond)
        return cond and (eval(body.body, env, {
          loop = k,
          excep = k0.excep
        }, (function(env)
          return whilef(env)
        end))) or k()
      end)
    end
    return whilef(env)
  elseif "repeat" == _exp_0 then
    local repf
    repf = function(env)
      return eval(body.body, env, {
        loop = k,
        excep = k0.excep
      }, function(env)
        return eval_exp(body.cond, env, k0, function(cond)
          if cond then
            return k()
          else
            return repf(env)
          end
        end)
      end)
    end
    return repf(env)
  elseif "for" == _exp_0 then
    local var, cnt, to, step
    var, cnt, to, step, body = body.var, body.cnt, body.to, body.step, body.body
    return eval_exp(cnt, env, k0, function(cnt)
      env[var] = cnt
      step = step or 1
      return eval_exp(step, env, k0, function(step)
        return eval_exp(to, env, k0, function(forend)
          local forf
          forf = function(e1)
            if not (e1[var] > forend) then
              return eval(body, e1, {
                loop = k,
                excep = k0.excep
              }, function(e2)
                cnt = cnt + step
                e2[var] = cnt
                for k, v in pairs(e2) do
                  if env[k] then
                    env[k] = v
                  end
                end
                return forf(e2)
              end)
            else
              return k()
            end
          end
          return forf(env)
        end)
      end)
    end)
  elseif "iter" == _exp_0 then
    local init = { }
    local namelist, explist
    namelist, explist, body = body.namelist, body.explist, body.body
    return eval_varlist({
      "_f",
      "_s",
      "_var"
    }, explist, init, env, k0, function()
      local _f, _s, _var
      _f, _s, _var = init._f, init._s, init._var
      local iterf
      iterf = function(_var)
        return function(env)
          return eval_funcall(_f, {
            _s,
            deeval_str(_var)
          }, env, k0, function(res)
            return eval_varlist(namelist, (function()
              local _accum_0 = { }
              local _len_0 = 1
              for _index_0 = 1, #res do
                local x = res[_index_0]
                _accum_0[_len_0] = deeval_str(x)
                _len_0 = _len_0 + 1
              end
              return _accum_0
            end)(), env, env, k0, function()
              do
                _var = env[namelist[1]]
                if _var then
                  return eval(body, env, {
                    loop = k,
                    excep = k0.excep,
                    fun = k0.fun
                  }, iterf(_var))
                else
                  return k()
                end
              end
            end)
          end)
        end
      end
      return (iterf(_var))(env)
    end)
  elseif "if" == _exp_0 then
    local cond, elsebody
    cond, body, elsebody = body.cond, body.body, body.elsebody
    return eval_exp(cond, env, k0, function(cond)
      if cond then
        return eval(body, env, k0, k)
      elseif elsebody then
        return eval((elsebody[1].label == "if" and elsebody or elsebody[1]), env, k0, k)
      else
        return k()
      end
    end)
  end
end
kinit = {
  loop = function()
    return error("not in loop")
  end,
  excep = function()
    return error("call throw out of try-catch")
  end
}
eval = function(syntaxtree, env, k0, k)
  if env == nil then
    env = { }
  end
  if k0 == nil then
    k0 = (cp_tbl(kinit))
  end
  if k == nil then
    k = noop
  end
  if not (syntaxtree[1]) then
    return k(env)
  else
    syntaxtree = cp_tbl(syntaxtree)
    env = setmetatable((cp_tbl(env)), {
      __index = __ENV,
      __mode = 'kv'
    })
    return eval_body(remove(syntaxtree, 1), env, k0, function()
      return eval(syntaxtree, env, k0, k)
    end)
  end
end
return eval
