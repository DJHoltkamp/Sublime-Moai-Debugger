package.path = "/Users/davidholtkamp/Library/Application Support/Sublime Text 3/Packages/Moai Debugger/lua_launchers/?.lua;" .. package.path
require("debugger")("127.0.0.1", 9000, nil, nil, nil, "/Users/davidholtkamp/Dropbox/Deimos/Lua/lua/")
dofile("main.lua")
