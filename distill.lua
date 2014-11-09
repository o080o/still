fname, outname = arg[1], arg[2]
assert(fname, "Usage: lua distill.lua FILENAME [TARGET]")
outname = outname or fname..".json"
local parser = require("still")
local content = io.open(fname):read("*all")
local jsonout = parser:parse(content)
io.open(outname, "w"):write( jsonout ):close()

