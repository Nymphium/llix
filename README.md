# llix - Lightweight Lua Interpreter eXtended
`/ˈliɹ.ɪks/`

## concept
llix is Lua interpreter, added the ***try-catch*** syntax.
```Lua
try
	if tonumber(io.read()) < 5 then
		throw()
	end
catch
	io.stderr:write "Error\n"
end
```

### --[[*New*]] type annotation
You can annotate type of variable:
```Lua
-- in comment, `T@ <varname> :: <type>`
local num = 3 -- T@ num :: number

-- yes, you fail
local notstr = 0 -- T@ notstr :: string
```

only allowed to annotate monomorphic type and can't do about functions' return value.

## usage
```
$ llix
llix - Lightweight Lua Interpreter eXtended (MoonScript version 0.4.0 on Lua 5.3)

> try
>> if tonumber(io.read()) < 5 then
>> throw()
>> end
>> print "foo"
>> catch
>> io.stderr:write "Error\n"
>> end
3
Error
>
```
## installation
`$ luarocks install --local llix`

or

```
$ git clone https://github.com/Nymphium/llix
$ cd llix
$ luarocks install --local make llix-build-1.rockspec
```

## requirement
- Lua
 + [MoonScript](http://moonscript.org)
 + [inspect](https://github.com/kikito/inspect.lua)
 + [LuLPeg](https://github.com/pygy/LuLPeg)
 + [lua-linenoise](https://github.com/hoelzro/lua-linenoise)

## TODO
```
$ grep -E '\b(TODO)|(XXX)\b' *
```
## License
[MIT License](http://opensource.org/licenses/mit-license.php)
