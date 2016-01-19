package = "llix"
version = "build-1"
source = {
	url = "git://github.com/nymphium/llix"
}
description = {
	summary = "llix, Lightweight Lua Interpreter eXtended",
	detailed = [[llix is Lua interpreter, added the try-catch syntax.]],
	homepage = "https://github.com/nymphium/llix",
	license = "MIT"
}
dependencies = {}
build = {
   type = "builtin",
   modules = {
      ["llix.eval"] = "llix/eval.lua",
      ["llix.parse"] = "llix/parse.lua"
   },
   install = {
      bin = {"bin/llix"}
   }
}
