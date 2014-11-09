#still

###### A Moonshine VM distilling script written in pure lua

A distilling script for the moonshine vm. Converts lua 5.1 bytecode into json format

The file exports a parser object that can be used (with method syntax) to parse a string containing lua5.1 bytecode into a json string to be loaded into the moonshine vm. (note that the moonshine vm is rather picky about its json format, so be sure not to add extra new line characters at the end of the string, ex. by using print, or editing the resulting file in any sane text editor)

Usage:
`lua distill.lua input [target]`

example script using the parser:
```
local parser = require("still")
local content = io:open("luac.out):read("*all"):close()
local json_output = parser:parse(content)
io.open("output.json","w"):write(json_output):close()
```
