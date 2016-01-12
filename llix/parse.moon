import P,S, V,
	C, Cb, Cc, Cg, Cs, Cmt, Ct, Cf, Cp
	locale, match from require'lpeg'
import insert, remove from table

locale = locale!
K = (k) -> P(k) * -(locale.alnum + P'_')
CV = (pat) -> C V pat
CK = (pat) -> C K pat
CP = (pat) -> C P pat
CS = (pat) -> C S pat
CtV = (pat) -> Ct V pat
opt = (pat) -> (pat)^-1
ast = (pat) -> (pat)^0

-- spaces(a, b, c, ..., z) ==> a * V'Space' * b * V'Space' * c * ... * V'Space' * z
spaces = (head, ...) ->
	args = {...}

	not args[1] and head or head * V'Space' * spaces unpack args

keywords = (head, ...) ->
	args = {...}

	not args[1] and K(head) or K(head) + keywords unpack args

-- lbl_tbl(lbl, l1, l2, l3, ..., ln) (c1, c2, c3, ..., cn, cn+1, ...) ==> {label: lbl, l1: c1, l2: c2, ..., ln:cn, cn+1, cn+2, ...}
lbl_tbl = (lbl, ...) ->
	tags = {...}
	(...) -> with args = {label: lbl, ...}
		if type(args[1]) == "string" and #args[1] < 1
			remove args, 1
		else for i = 1, #args
			if t = tags[i]
				cont = args[i]
				args[t] = cont if #cont > 0
				args[i] = nil

-- unificate ({else{if}}, {else{if}}, ... ==> {else{if{else{if ...)
gen_nesttbl = (...) ->
	gn = (...) ->
		args = {...}
		with tail = remove args
			if args[1]
				insert(args[#args][1], tail)

				return gen_nesttbl unpack args

	t = gn ...

	type(t) == "table" and t or nil

-- "x", "+", "y", "-", "z"... ==> {op:"-", {op:"+", "x", "y"}, "z"}
gen_binoptbl = (a, b, c, ...) ->
	unless c
		b and {op:a, label:"exp", b} or a
	else gen_binoptbl {op:b, label:"exp", a, c}, ...

-- "not", "not", "not", "true" ==> {op: "not", {op:"not", {op: "not", "true"}}}
gen_unoptbl = (...) ->
	args = {...}
	val = remove args
	t = {op:remove(args), label:"exp", val}

	insert args, t

	#args < 2 and (t.op and t or val) or gen_unoptbl unpack args

gen_exp = (next, pat) -> V(next) * ast(V'Space' * pat * V'Space' * V(next)) / gen_binoptbl

gen_tblaccess = (a, ...) -> #{...} < 1 and a or {label: "tableaccess", (type(a) == "string" and a\gsub("\"", "") or a), gen_tblaccess ...}

-- parse(Funcbody) ==> {....}, {....}, {....} constantly make three tables
-- normalize_funcbody = (...) ->
	-- body = {...}
	-- args = remove body, 1
	-- cont = remove body

	-- if cont.label == "return"
		-- args, body, cont
	-- else
		-- insert(body, cont)
		-- args, body


llix = P{
	opt(P'#' * ast(1 - P'\n') * P'\n') * V'Space' * CtV'Chunk' * V'Space' * -P(1)
	Keywords: keywords 'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for',
		'function', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat', 'return', 'then', 'true', 'until', 'while', 'try', 'catch'

	Chunk: ast(V'Space' * V'Stat' * opt(V'Space' * P';')) * opt(V'Space' * V'Laststat' * opt(V'Space' * P';'))
	Block: V'Chunk'
	Space: ast(locale.space + V'Comment')
	Comment:
		(P'--' * V'Longstring' +
		P'--' * ast(P(1) - P'\n') * (P'\n' + -P(1))) /->

	Number:
		P'0x' * (locale.xdigit)^1 * -(locale.alnum + P'_') +
		locale.digit^1 * opt(P'.' * locale.digit^1) * opt(S'eE' * locale.digit^1) * -(locale.alnum + P'_') +
		P'.' * locale.digit^1 * opt(S'eE' * locale.digit^1) * -(locale.alnum + P'_')

	Longstring:
		C P{
			V'open' * C(ast(P(1) - V'closeeq')) * V'close' / 2
			open: '[' * Cg(ast(P'='), 'init') * P'[' * opt(P'\n')
			close: ']' * C(ast(P'=')) * ']'
			closeeq: Cmt(V'close' * Cb'init', (_, _, a, b) -> a == b)
		}

	String:
		(((P"\"" * C(ast(P"\\" * P(1) + (1 - P"\""))) * P"\"") +
		(P"'" * C(ast(P"\\" * P(1) + (1 - P"'"))) * P"'")) / (str) -> "\"#{str}\"") +
		(V"Longstring" / (a) -> a)

	Fieldsep: P',' + P';'
	Name: (locale.alpha + P'_') * ast(locale.alnum + P'_') - V'Keywords'
	Stat:
		spaces(K'do', V'Block', K'end') / lbl_tbl'do' +
		spaces(K'while', V'Exp', K'do', CtV'Block', K'end') / lbl_tbl('while', 'cond', 'body') +
		spaces(K'repeat', CtV'Block', K'until', V'Exp') / lbl_tbl('repeat', 'body', 'cond') +
		spaces(K'if', V'Exp', K'then', CtV'Block', (ast(spaces(K'elseif', V'Exp', K'then', CtV'Block') / lbl_tbl('if', 'cond', 'body', 'elsebody') / lbl_tbl'else'  * V'Space') *
			opt(K'else' * V'Space' * CtV'Block' / lbl_tbl'else' * V'Space')/((e) -> e) / gen_nesttbl), K'end') / lbl_tbl('if', 'cond', 'body', 'elsebody') +
		spaces(K'for', CV'Name', P'=', V'Exp', P',', V'Exp') * spaces(opt(V'Space' * P',' * V'Space' * V'Exp')/((e) -> e) , K'do', CtV'Block', K'end') / lbl_tbl('for', 'var', 'cnt', 'to', 'step', 'body') +
		spaces(K'for', CtV'Namelist', K'in', CtV'Explist', K'do', CtV'Block', K'end') / lbl_tbl('iter', 'namelist', 'explist', 'body') +
		spaces(K'function', V'Funcname', V'Funcbody', K'end') / lbl_tbl('funcdef', 'name', 'args', 'body') +
		spaces(K'local', K'function', CV'Name', V'Funcbody', K'end') / lbl_tbl('localfuncdef', 'name', 'args', 'body') +
		K'local' * V'Space' * CtV'Namelist' * opt(V'Space' * P'=' * V'Space' * CtV'Explist') / lbl_tbl'localvarlist' +
		spaces(CtV'Varlist', P'=', CtV'Explist') / lbl_tbl'varlist' +
		V'Funcall' +
		spaces(K'try', CtV'Block', K'catch', CtV'Block', K'end') / lbl_tbl('try', 'body', 'catchbody')

	Laststat: K'return' * (opt(V'Space' * V'Explist')) / lbl_tbl'return' + K'break' / -> label:'break'
	Namelist: CV'Name' * ast(V'Space' * P',' * V'Space' * CV'Name')
	Varlist: V'Var' * ast(V'Space' * P',' * V'Space' * V'Var')
	Value:
		CK'nil' +
		CK'false' +
		CK'true' +
		CV'Number' +
		V'String' +
		CP'...' +
		V'Funcdef' +
		V'Tableconstructor' +
		V'Funcall' +
		V'Var' +
		spaces(P'(', V'Exp', P')')

	Exp: V'lor'
	lor: gen_exp 'land', CK'or'
	land: gen_exp 'cmp', CK'and'
	cmp: gen_exp 'or', C(P'<=' + P'>=' + P'~=' + P'==' + S'<>')
	or: gen_exp 'xor', CP'|'
	xor: gen_exp 'and', CP'~'
	and: gen_exp 'shift', CP'&'
	shift: gen_exp 'cnct', C(P'<<' + P'>>')
	cnct: gen_exp 'term', CP'..'
	term: gen_exp 'fact', CS'+-'
	fact: gen_exp 'hat', C(P'//' + S'*/%')
	hat: gen_exp 'expend', CP'^'
	expend: ast(C((K'not') + S'-~#') * V'Space') * V'Value' / gen_unoptbl  + gen_exp 'Value', V'Exp'
	Explist: V'Exp' * ast(V'Space' * P',' * V'Space' * V'Exp')
	Index:
		spaces(P'[', V'Exp', P']') +
		P'.' * V'Space' * (CV'Name' / (n) -> "\"#{n}\"")

	Colonfunc: P':' * V'Space' * CV'Name' * V'Space' * V'Callargs' / lbl_tbl 'colonfunc', 'func', 'args'
	Call: V'Callargs' + V'Colonfunc' -- * V'Space' * V'Callargs' / lbl_tbl'colonfunc'
	Prefix: spaces(P'(', V'Exp', P')') + CV'Name'
	Suffix: V'Call' + V'Index'
	Var: (V'Prefix' * ast(V'Space' * V'Suffix' * #(V'Space' * V'Suffix')) * V'Space' * V'Index' + CV'Name') / gen_tblaccess
	Funcall: V'Prefix' * ast(V'Space' * V'Suffix' * #(V'Space' * V'Suffix')) / gen_tblaccess * V'Space' * V'Call' / lbl_tbl'funcall'
	Funcname: CV'Name' * ast(V'Space' * P'.' * V'Space' * CV'Name') * opt(V'Space' * P':' * V'Space' * CV'Name')

	Callargs:
		Ct(P'(' * V'Space' * opt(V'Explist' * V'Space') * P')' +
		(V'Tableconstructor' + V'String'))

	Funcdef: K'function' * V'Space' * V'Funcbody' * V'Space' * K'end' / lbl_tbl('annonymousfuncdef', 'args', 'body')
	Funcbody: spaces P'(', (opt(V'Parlist') / lbl_tbl'args'), P')', CtV'Block'

	Parlist: (V'Namelist' * opt(V'Space' * P',' * V'Space' * CP'...') + CP'...')
	Tableconstructor: P'{' * V'Space' * (opt(V'fieldlist' * V'Space') / lbl_tbl'constructor') * P'}'
	fieldlist: V'Field' * ast(V'Space' * V'Fieldsep' * V'Space' * V'Field') * opt(V'Space' * V'Fieldsep')
	Field:
		Ct(spaces(P'[', CtV'Exp', P']', P'=', V'Exp')) +
		Ct(spaces(CV'Name', P'=', V'Exp')) + V'Exp'
}

(msg) ->
	tree = {llix\match msg}

	if h = tree[1]
		#tree > 1 and tree or h
	else nil, "Failed to parse"

