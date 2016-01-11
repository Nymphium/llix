# llix - Lightweight Lua Interpreter eXtended
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
$ luarocks install --local make
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
