local P, S, V, C, Cb, Cc, Cg, Cs, Cmt, Ct, Cf, Cp, locale, match
do
  local _obj_0 = require('lpeg')
  P, S, V, C, Cb, Cc, Cg, Cs, Cmt, Ct, Cf, Cp, locale, match = _obj_0.P, _obj_0.S, _obj_0.V, _obj_0.C, _obj_0.Cb, _obj_0.Cc, _obj_0.Cg, _obj_0.Cs, _obj_0.Cmt, _obj_0.Ct, _obj_0.Cf, _obj_0.Cp, _obj_0.locale, _obj_0.match
end
local insert, remove
do
  local _obj_0 = table
  insert, remove = _obj_0.insert, _obj_0.remove
end
locale = locale()
local K
K = function(k)
  return P(k) * -(locale.alnum + P('_'))
end
local CV
CV = function(pat)
  return C(V(pat))
end
local CK
CK = function(pat)
  return C(K(pat))
end
local CP
CP = function(pat)
  return C(P(pat))
end
local CS
CS = function(pat)
  return C(S(pat))
end
local CtV
CtV = function(pat)
  return Ct(V(pat))
end
local opt
opt = function(pat)
  return (pat) ^ -1
end
local ast
ast = function(pat)
  return (pat) ^ 0
end
local foldmap
foldmap = function(self, f, val)
  for _index_0 = 1, #self do
    local i = self[_index_0]
    val = f(val, i)
  end
  return val
end
local spaces
spaces = function(head, ...)
  local args = {
    ...
  }
  return not args[1] and head or head * V('Space') * spaces(table.unpack(args))
end
local keywords
keywords = function(head, ...)
  local args = {
    ...
  }
  return not args[1] and K(head) or K(head) + keywords(table.unpack(args))
end
local lbl_tbl
lbl_tbl = function(lbl, ...)
  local tags = {
    ...
  }
  return function(...)
    do
      local args = {
        label = lbl,
        ...
      }
      if type(args[1]) == "string" and #args[1] < 1 then
        remove(args, 1)
      else
        for i = 1, #args do
          do
            local t = tags[i]
            if t then
              local cont = args[i]
              if #cont > 0 then
                args[t] = cont
              end
              args[i] = nil
            end
          end
        end
      end
      return args
    end
  end
end
local gen_nesttbl
gen_nesttbl = function(...)
  local gn
  gn = function(...)
    local args = {
      ...
    }
    do
      local tail = remove(args)
      if args[1] then
        insert(args[#args][1], tail)
        return gen_nesttbl(table.unpack(args))
      end
      return tail
    end
  end
  local t = gn(...)
  return type(t) == "table" and t or nil
end
local gen_binoptbl
gen_binoptbl = function(a, b, c, ...)
  if not (c) then
    return b and {
      op = a,
      label = "exp",
      b
    } or a
  else
    return gen_binoptbl({
      op = b,
      label = "exp",
      a,
      c
    }, ...)
  end
end
local gen_unoptbl
gen_unoptbl = function(...)
  local args = {
    ...
  }
  local val = remove(args)
  local t = {
    op = remove(args),
    label = "exp",
    val
  }
  insert(args, t)
  return #args < 2 and (t.op and t or val) or gen_unoptbl(table.unpack(args))
end
local gen_exp
gen_exp = function(next, pat)
  return V(next) * ast(V('Space') * pat * V('Space') * V(next)) / gen_binoptbl
end
local gen_tblaccess
gen_tblaccess = function(a, ...)
  return #{
    ...
  } < 1 and a or {
    label = "tableaccess",
    (type(a) == "string" and a:gsub("\"", "") or a),
    gen_tblaccess(...)
  }
end
local parse
local llix = P({
  opt(P('#') * ast(1 - P('\n')) * P('\n')) * V('Space') * CtV('Chunk') * V('Space') * -P(1),
  Keywords = keywords('and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for', 'function', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat', 'return', 'then', 'true', 'until', 'while', 'try', 'catch', 'delim', 'continue'),
  Chunk = ast(V('Space') * V('Stat') * opt(V('Space') * P(';'))) * opt(V('Space') * V('Laststat') * opt(V('Space') * P(';'))),
  Block = V('Chunk'),
  Space = ast(locale.space + V('Comment')),
  Comment = (P('--') * V('Longstring') + P('--') * V('Space') * (V('TACore') + ast(P(1) - (P('\n') + P('T@')))) * (P('\n') + -P(1))),
  TACore = (function()
    do
      local types = {
        "number",
        "string",
        "table",
        "function",
        "coroutine",
        "userdata",
        "nil"
      }
      return spaces(P('T@'), CV('Name'), P('::'), C(foldmap(types, (function(self, x)
        return P(x) + self
      end), #(-P('.'))))) / function(n, t)
        return (parse("assert((type(" .. tostring(n) .. ") == \"" .. tostring(t) .. "\"), [[`" .. tostring(n) .. "' is not \"" .. tostring(t) .. "\"]])"))[1]
      end
    end
  end)(),
  Number = P('0x') * (locale.xdigit) ^ 1 * -(locale.alnum + P('_')) + locale.digit ^ 1 * opt(P('.') * locale.digit ^ 1) * opt(S('eE') * locale.digit ^ 1) * -(locale.alnum + P('_')) + P('.') * locale.digit ^ 1 * opt(S('eE') * locale.digit ^ 1) * -(locale.alnum + P('_')),
  Longstring = C(P({
    V('open') * C(ast(P(1) - V('closeeq'))) * V('close') / 2,
    open = '[' * Cg(ast(P('=')), 'init') * P('[') * opt(P('\n')),
    close = ']' * C(ast(P('='))) * ']',
    closeeq = Cmt(V('close') * Cb('init'), function(_, _, a, b)
      return a == b
    end)
  })),
  String = (((P("\"") * C(ast(P("\\") * P(1) + (1 - P("\"")))) * P("\"")) + (P("'") * C(ast(P("\\") * P(1) + (1 - P("'")))) * P("'"))) / function(str)
    return "\"" .. tostring(str) .. "\""
  end) + (V("Longstring") / function(a)
    return a
  end),
  Fieldsep = P(',') + P(';'),
  Name = (locale.alpha + P('_')) * ast(locale.alnum + P('_')) - V('Keywords'),
  Stat = spaces(K('do'), V('Block'), K('end')) / lbl_tbl('do') + spaces(K('while'), V('Exp'), K('do'), CtV('Block'), K('end')) / lbl_tbl('while', 'cond', 'body') + spaces(K('repeat'), CtV('Block'), K('until'), V('Exp')) / lbl_tbl('repeat', 'body', 'cond') + spaces(K('if'), V('Exp'), K('then'), CtV('Block'), (ast(spaces(K('elseif'), V('Exp'), K('then'), CtV('Block')) / lbl_tbl('if', 'cond', 'body', 'elsebody') / lbl_tbl('else') * V('Space')) * opt(K('else') * V('Space') * CtV('Block') / lbl_tbl('else') * V('Space')) / (function(e)
    return e
  end) / gen_nesttbl), K('end')) / lbl_tbl('if', 'cond', 'body', 'elsebody') + spaces(K('for'), CV('Name'), P('='), V('Exp'), P(','), V('Exp')) * spaces(opt(V('Space') * P(',') * V('Space') * V('Exp')) / (function(e)
    return e
  end), K('do'), CtV('Block'), K('end')) / lbl_tbl('for', 'var', 'cnt', 'to', 'step', 'body') + spaces(K('for'), CtV('Namelist'), K('in'), CtV('Explist'), K('do'), CtV('Block'), K('end')) / lbl_tbl('iter', 'namelist', 'explist', 'body') + spaces(K('function'), V('Funcname'), V('Funcbody'), K('end')) / lbl_tbl('funcdef', 'name', 'args', 'body') + spaces(K('local'), K('function'), CV('Name'), V('Funcbody'), K('end')) / lbl_tbl('localfuncdef', 'name', 'args', 'body') + K('local') * V('Space') * CtV('Namelist') * opt(V('Space') * P('=') * V('Space') * CtV('Explist')) / lbl_tbl('localvarlist') + spaces(CtV('Varlist'), P('='), CtV('Explist')) / lbl_tbl('varlist') + V('Funcall') + V('Delim') + spaces(K('continue'), V('Exp')) / lbl_tbl('continue') + spaces(K('try'), CtV('Block'), K('catch'), CtV('Block'), K('end')) / lbl_tbl('try', 'body', 'catchbody'),
  Laststat = K('return') * (opt(V('Space') * V('Explist'))) / lbl_tbl('return') + K('break') / function()
    return {
      label = 'break'
    }
  end,
  Namelist = CV('Name') * ast(V('Space') * P(',') * V('Space') * CV('Name')),
  Varlist = V('Var') * ast(V('Space') * P(',') * V('Space') * V('Var')),
  Value = CK('nil') + CK('false') + CK('true') + CV('Number') + V('String') + CP('...') + V('Funcdef') + V('Delim') + V('Tableconstructor') + V('Funcall') + V('Var') + spaces(P('('), V('Exp'), P(')')),
  Exp = V('lor'),
  lor = gen_exp('land', CK('or')),
  land = gen_exp('cmp', CK('and')),
  cmp = gen_exp('or', C(P('<=') + P('>=') + P('~=') + P('==') + S('<>'))),
  ["or"] = gen_exp('xor', CP('|')),
  xor = gen_exp('and', CP('~')),
  ["and"] = gen_exp('shift', CP('&')),
  shift = gen_exp('cnct', C(P('<<') + P('>>'))),
  cnct = gen_exp('term', CP('..')),
  term = gen_exp('fact', CS('+-')),
  fact = gen_exp('hat', C(P('//') + S('*/%'))),
  hat = gen_exp('expend', CP('^')),
  expend = ast(C((K('not')) + S('-~#')) * V('Space')) * V('Value') / gen_unoptbl + gen_exp('Value', V('Exp')),
  Explist = V('Exp') * ast(V('Space') * P(',') * V('Space') * V('Exp')),
  Index = spaces(P('['), V('Exp'), P(']')) + P('.') * V('Space') * (CV('Name') / function(n)
    return "\"" .. tostring(n) .. "\""
  end),
  Colonfunc = P(':') * V('Space') * CV('Name') * V('Space') * V('Callargs') / lbl_tbl('colonfunc', 'func', 'args'),
  Call = V('Callargs') + V('Colonfunc'),
  Prefix = spaces(P('('), V('Exp'), P(')')) + CV('Name'),
  Suffix = V('Call') + V('Index'),
  Var = (V('Prefix') * ast(V('Space') * V('Suffix') * #(V('Space') * V('Suffix'))) * V('Space') * V('Index') + CV('Name')) / gen_tblaccess,
  Funcall = V('Prefix') * ast(V('Space') * V('Suffix') * #(V('Space') * V('Suffix'))) / gen_tblaccess * V('Space') * V('Call') / lbl_tbl('funcall'),
  Funcname = CV('Name') * ast(V('Space') * P('.') * V('Space') * CV('Name')) * opt(V('Space') * P(':') * V('Space') * CV('Name')),
  Callargs = Ct(P('(') * V('Space') * opt(V('Explist') * V('Space')) * P(')') + (V('Tableconstructor') + V('String'))),
  Funcdef = K('function') * V('Space') * V('Funcbody') * V('Space') * K('end') / lbl_tbl('annonymousfuncdef', 'args', 'body'),
  Funcbody = spaces(P('('), (opt(V('Parlist')) / lbl_tbl('args')), P(')'), CtV('Block')),
  Delim = spaces(K('delim'), CtV('Block'), P('end')) / lbl_tbl('delim'),
  Parlist = (V('Namelist') * opt(V('Space') * P(',') * V('Space') * CP('...')) + CP('...')),
  Tableconstructor = P('{') * V('Space') * (opt(V('fieldlist') * V('Space')) / lbl_tbl('constructor')) * P('}'),
  fieldlist = V('Field') * ast(V('Space') * V('Fieldsep') * V('Space') * V('Field')) * opt(V('Space') * V('Fieldsep')),
  Field = Ct(spaces(P('['), CtV('Exp'), P(']'), P('='), V('Exp'))) + Ct(spaces(CV('Name'), P('='), V('Exp'))) + V('Exp')
})
parse = function(msg)
  local tree = {
    llix:match(msg)
  }
  do
    local h = tree[1]
    if h then
      return #tree > 1 and tree or h
    else
      return nil, "Failed to parse"
    end
  end
end
return parse
