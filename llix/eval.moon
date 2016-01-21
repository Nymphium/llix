import insert, remove from table
_insert = insert

insert = (t, i, o) -> with t
	unless o then _insert t, i
	else _insert t, i, o

-- for debug
inspect = require"inspect"
iprint = (...) -> print (inspect {...})\match("^{ (.*) }$")
---

type_s = (obj) -> type(obj) == "string"
type_t = (obj) -> type(obj) == "table"
is_funcall = (t) -> type_t(t) and t.label == "funcall"
noop = => @
cp_tbl = (t) -> {k, (type_t v) and (cp_tbl v) or v for k, v in pairs t}

strmguard = (str, t) ->
	for i = 1, #t, 2
		unless type_s(t[i]) or type(t[i + 1]) == "function"
			error "strmguard failed"

		if str\match t[i]
			return t[i + 1]!

	t.default! if t.default

strip_string = (str) ->
	with dq = str\match "^\"(.*)\""
		unless dq then dq = str\gsub("^%[(=*)%[(.*)%]%1%]", "%2")

deeval_str = (x) ->
	if type_s x
		noop x\gsub "^(.*)", "\"%1\""
	else x or "nil"

binop_table = {"<=", ">=", "~=", "==", "<", ">", "..",
	"+", "-", "*", "/", "%", "^", ">>", "<<", "|", "&"}
binop = setmetatable {op, (load"return function(l, r) return l #{op} r end")! for op in *binop_table},
	__index:
		"//": (l, r) ->
			if r == 0
				-- XXX: why doesn't `error` work at repl ... ??
				-- io.stderr\write "attempt to divide by zero\n"
				error "attempt to divide by zero"
			else l // r
	__call: (op, left, right) => @[op] left, right

uniop_table = {"-", "~", "#", "not"}
uniop = setmetatable {op, (load"return function(e) return #{op}(e) end")! for op in *uniop_table},
	__index: (op) -> (load"return function(e) return #{op}(e) end")!
	__call: (op, e) => @[op] e

-- call stack
funstack =
	pushcresume: (fun) => coroutine.resume (insert @, {coroutine.create fun})[#@][1]
	stopregret: (ret) => coroutine.yield (insert @[#@], ret)[#@][1]
	pop: => remove @

-- environment initialize
__ENV =
	:_VERSION
	arg: cp_tbl arg
	:assert
	bit32: cp_tbl bit32
	:collectgarbage
	coroutine: cp_tbl coroutine
	debug: cp_tbl debug
	:dofile
	:error
	:getmetatable
	io: cp_tbl io
	:ipairs
	:load
	:loadfile
	:loadstring
	math: cp_tbl math
	:module
	:next
	os: cp_tbl os
	:package
	:print
	:pairs
	:pcall
	:rawequal
	:rawget
	:rawset
	:require
	:select
	:setmetatable
	string: cp_tbl string
	table: cp_tbl table
	:tonumber
	:tostring
	:type
	:unpack
	utf8: cp_tbl utf8
	:xpcall

__ENV.package.loaded = with __ENV
	._G = __ENV
	._ENV = __ENV

-- for mutual recursive functions
local *

-- tbl = {"t", "i"} ==> env["t"]["i"]
-- `is_dec == true` ==> env["t"], "i"
expand_tbl = (tbl, is_dec, env, k0, k) ->
	if type_t tbl[2]
		switch tbl[2].label
			when nil
				env._llix_tmp_tbl = env._llix_tmp_tbl and env._llix_tmp_tbl[tbl[2][1]] or env[tbl[1]][tbl[2][1]]

				expand_tbl tbl[2], is_dec, env, k0, k
			when "exp" then eval_exp tbl[2], env, k0, (e) ->
				tbl[2] = e
				expand_tbl tbl, is_dec, env, k0, k
			when "funcall" then eval_funcall tbl[2][1], tbl[2][2], env, k0, (e) ->
				tbl[2] = remove e, 1
				expand_tbl tbl, is_dec, env, k0, k
	else
		f = (e) ->
			if tmpt = env._llix_tmp_tbl then tmpt[e]
			else
				if p = env[tbl[1]] then p[e]
				else
					-- XXX: why doesn't `error` work at repl ... ?????
					-- io.stderr\write "attempt to index a nil valie (local '#{tbl[1]}')\n"
					error "attempt to index a nil valie (local '#{tbl[1]}')"

		unless is_dec
			if type(tbl[2]) == "number" then k f tbl[2]
			elseif m = tbl[2]\match "^\"(.*)\"$" then k f m
			elseif m = tonumber tbl[2] then k f m
			else eval_exp tbl[2], env, k0, (e) -> k f e
		else k (env._llix_tmp_tbl and env._llix_tmp_tbl or env[tbl[1]]), tbl[2]


-- t = {x, y, z} ==> {evaled_ x, evaled_y, evaled_z}
eval_tbl = (fields, pos = 1, env, k0, k) ->
	unless fields[1] then k fields
	else
		local key, val
		head = remove(fields, 1)
		tblf = (val, pos) -> (key) ->
			eval_exp val, env, k0, (x) -> eval_tbl fields, pos, env, k0, (y) ->
				k with y do y[key] = x

		switch type head
			when "table"
				switch head.label
					when not fields[1] and "funcall"
						return eval_funcall head[1], head[2], env, k0, (t) -> k t
					when "tableaccess", "constructor"
						key = pos
						val = (head.label == "tableaccess" and head or label: "constructor")
						pos += 1
					when nil
						key = type_t(head[1]) and remove(head[1]) or deeval_str head[1]
						val = head[2]

				eval_exp key, env, k0, (tblf val, pos)
			when "string"
				key = pos
				val = head
				pos += 1

				if val == "..." and not fields[1]
					eval_exp key, env, k0, (key) -> eval_args env[head], env, k0, (vars) ->
							eval_tbl fields, pos, env, k0, (y) ->
								k with y do for i = 1, #vars do y[i + key] = vars[i]
				else (tblf val, pos) key

eval_exp = (exp, env, k0, k) -> switch type(exp)
	when "string" then k strmguard exp, {
		"^%d", -> tonumber exp
		"^%.%d", -> tonumber exp
		"^nil$", -> nil
		"^true$", -> true
		"^false$", -> false
		"^[_a-zA-Z]", -> env[exp] or nil
		"^%.%.%.$", -> eval_args env[exp][1], env, k0, (e) -> k remove e, 1
		default: -> strip_string exp
	}
	when "table" then switch exp.label
		when "constructor"
			exp.label = nil

			eval_tbl exp, _, env, k0, k
		when "funcall" then eval_funcall exp[1], exp[2], env, k0, (t) -> k (remove t, 1)
		when "annonymousfuncdef" then eval_funcdef exp, env, k0, k
		when "tableaccess" then k ({expand_tbl exp, _, env, k0, noop})[1]
		when "exp"
			import op from exp

			if exp[2] then switch exp.op
				when "or" then eval_exp exp[1], env, k0, (do_l) ->
					if do_l then k do_l
					else eval_exp exp[2], env, k0, k
				when "and" then eval_exp exp[1], env, k0, (do_l) ->
					if do_l then eval_exp exp[2], env, k0, k
					else k do_l
				else eval_exp exp[1], env, k0, (left) ->
					eval_exp exp[2], env, k0, (right) ->
						k binop op, left, right
			else eval_exp exp[1], env, k0, (exp) -> k uniop op, exp
		else k exp
	else k exp

-- TODO: local recursive function such as: local f = function...
eval_funcdef = (def, env, k0, k) ->
	nenv = cp_tbl env
	def.name or= ""

	nenv[def.name] = (...) ->
		gargs = {...}

		if args = def.args
			for i = 1, #args
				unless args[i] == "..."
					nenv[args[i]] = remove gargs, 1
				else
					nenv[args[i]] = cp_tbl gargs
					break

		funstack\pushcresume -> eval def.body, nenv, k0
		ret = funstack\pop!

		ret[2] and unpack ret[2] or nil

	k nenv[def.name]

eval_args = (arglist, env, k0, k) ->
	unless arglist[1]
		k arglist
	else
		head = remove arglist, 1
		argf = -> eval_exp head, env, k0, (x) ->
			eval_args arglist, env, k0, (y) ->
				_insert y, 1, x
				k y

		unless arglist[1]
			if is_funcall head
				eval_funcall head[1], head[2], env, k0, (x) ->
					eval_args arglist, env, k0, (y) ->
						k with y do for i = 1, #x do _insert y, x[i]
			elseif head == "..."
				eval_args env[head], env, k0, (t) -> k t
			else argf!
		else argf!

-- TODO redefine load and require
-- TODO: f():g()
eval_funcall = (func, args, env, k0, k) ->
	if func == "throw" then return k0.excep args[1], env

	run = (args) -> (func) ->
		-- TODO setmetatable don't work well
		eval_args args, env, k0, (fmt_args) ->
			-- if #fmt_args < 1
				-- k {func _}
			-- else
				-- TODO: why `k` "ignore first "nil"
			k {func unpack fmt_args}

	if type_t func
		return switch func.label
			when "tableaccess" then expand_tbl func, _, env, k0, (func) ->
				((args) -> (run args) (env[func] and env[func] or func)) with args
					if .label == "colonfunc"
						s = func
						func = func[.func]
						args = .args or {}
						_insert args, 1, s
			when "annonymousfuncdef" then eval_funcdef func, env, k0, run args
			when "funcall" then eval_exp func, env, k0, (f) -> (run args) f

	((args) -> (run args) (env[func] and env[func] or func)) with args
		if .label == "colonfunc"
			s = func

			if (type_s func) and (func\match"^\"" or func\match"^%[=*%[")
				func = string[.func]
			else func = env[func][.func]

			args = .args or {}
			_insert args, 1, s

-- insert all the elements which is evaled in body to ret
eval_return = (body, ret = {}, env, k0, k) ->
	unless body[1]
		if funstack[1] then k funstack\stopregret ret
		else k ret
	else
		body = cp_tbl body
		head = remove body, 1

		if not body[1] and is_funcall head
			eval_funcall head[1], head[2], env, k0, (t) ->
				for i in *t
					_insert ret, i
				eval_return body, ret, env, k0, k
		else eval_exp head, env, k0, (x) -> eval_return body, insert(ret, x), env, k0, k

-- bind one of the element in right corresponding to the name in the left to regtbl
-- eval_varlist {a, b, c, ...}, {"a", "b", "c", ..}, regtbl,...
-- ==> regtbl = a: evaled_a, b: evaled_b, c: evaled_c
eval_varlist = (left, right = ["" for _ = 1, #left], regtbl, env, k0, k) ->
	f = (t) ->
		-- XXX: left to eval like this: t.x = n
		for i = 1, #left
			if rawget env, left[i] -- actually the element in env or not
				env[left[i]] = t[i]
			else regtbl[left[i]] = t[i]

	k if not right[2] and is_funcall regtbl[1]
		eval_funcall right[1][1], right[1][2], env, k0, f
	else eval_args right, env, k0, f

-- k0 ... table(excep(try-catch), loop(while/for/repeat))
eval_body = (body, env, k0, k) -> switch body.label
	when "try"
		k1 = loop: k0, excep: (args, env) ->
			eval_exp args, env, k0, (t) ->
				env._ERR = t
				eval body.catchbody, env, k0, noop
		k eval body.body, env, k1, (e) ->
			for k, v in pairs e do env[k] = v if env[k]
	when "do" then k eval body, (cp_tbl env), k0, (e) ->
		for k, v in pairs e do env[k] = v if env[k]
	when "return" then eval_return body, _, env, k0, k
	when "break" then k0.loop!
	when "varlist" then eval_varlist body[1], body[2], __ENV, env, k0, k
	when "localvarlist" then eval_varlist body[1], _, env, env, k0, ->
		eval_varlist body[1], body[2], env, env, k0, k
	when "funcall" then eval_funcall body[1], body[2], env, k0, k
	when "tableaccess" then expand_tbl body, _, env, k0, noop
	when "funcdef" then k eval_funcdef body, env, k0, (f) -> __ENV[body.name] = f
	when "localfuncdef" then k eval_funcdef body, env, k0, (f) -> env[body.name] = f
	when "while"
		whilef = (env) -> eval_exp body.cond, env, k0, (cond) ->
			cond and (eval body.body, env, {loop: k, excep: k0.excep}, ((env) -> whilef(env))) or k!
		whilef env
	when "repeat"
		repf = (env) -> eval body.body, env, {loop: k, excep: k0.excep}, (env) ->
			eval_exp body.cond, env, k0, (cond) -> if cond then k! else repf env
		repf env
	when "for"
		import var, cnt, to, step, body from body

		eval_exp cnt, env, k0, (cnt) ->
			env[var] = cnt
			step or= 1

			eval_exp step, env, k0, (step) -> eval_exp to, env, k0, (forend) ->
				forf = (e1) ->
					unless e1[var] > forend
						eval body, e1, {loop: k, excep: k0.excep}, (e2) ->
							cnt += step
							e2[var] = cnt
							for k, v in pairs e2 do env[k] = v if env[k]
							forf e2
					else k!
				forf env
	when "iter"
		init = {}
		import namelist, explist, body from body

		eval_varlist {"_f", "_s", "_var"}, explist, init, env, k0, ->
			import _f, _s, _var from init

			-- TODO: update environment every loop, also iter
			-- XXX: DON'T update explist
			iterf = (_var) -> (env) ->
				eval_funcall _f, {_s, deeval_str _var}, env, k0, (res) ->
					eval_varlist namelist, [deeval_str x for x in *res], env, env, k0, ->
						if _var = env[namelist[1]]
							eval body, env, {loop: k, excep: k0.excep, fun: k0.fun}, iterf _var
						else k!

			(iterf _var) env
	when "if"
		import cond, body, elsebody from body

		eval_exp cond, env, k0, (cond) ->
			if cond then eval body, env, k0, k
			elseif elsebody then eval (elsebody[1].label == "if" and elsebody or elsebody[1]), env, k0, k
			else k!

kinit =
	loop:  -> error "not in loop"
	excep: -> error "call throw out of try-catch"

eval = (syntaxtree, env = {}, k0 = (cp_tbl kinit), k = noop) ->
	unless syntaxtree[1]
		k env
	else
		syntaxtree = cp_tbl syntaxtree
		env = setmetatable (cp_tbl env), __index: __ENV, __mode: 'kv'
		eval_body remove(syntaxtree, 1), env, k0, -> eval syntaxtree, env, k0, k

eval

