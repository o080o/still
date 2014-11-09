-- create a table, then convert that table to JSON.

local Parser = {}
function Parser:parse(content, config)
	self.runConfig = config or {}
	local magicvalue = string.char(27,76,117,97)
	if content:sub(1,4) ~= magicvalue then
	end
	--print(content)
	assert(content:sub(1,4) == magicvalue, "Not a compiled luac bytecode file! magic number does not match")
	local version = content:byte(5)
	assert(version==tonumber('51',16), "The version is incompatible, Please use lua51 ")
	self:parseData(content)
	return json:encode(self.tree), self.tree, self.config --json lib appended to this lua file
end
function Parser:parseData(content)
	self.content = content
	self.ptr = 1
	self:readGlobalHeader()
	self.tree = self:read('chunk')
	self.runConfig = nil
end

Parser.types = { [0]="nil", [1]="boolean", [3]="number", [4]="string" }
local ParseTable = {}
function Parser:read(valType, ...)
	valType = valType or 'nil'
	assert(ParseTable[valType], "Attempt to read an unknown type!" .. tostring(valType))
	return ParseTable[valType](self,...)
end
function Parser:readList(valType, ...)
	local len = self:read('integer')
	local result, index = {}
	for i=1,len do
		table.insert(result, self:read(valType))
	end
	return result
end

function Parser:readGlobalHeader()
	self.config = {
		signature=self:read("bytestr",4),
		version=self:read("byte"),
		formatVersion=self:read('byte'),
		endianness=self:read('byte')~=0,
		sizes= {
			int=self:read('byte'),
			size_t=self:read('byte'),
			instruction=self:read('byte'),
			number=self:read('byte')
		},
		integral=self:read('byte')
	}
	return self.config
end

function ParseTable.byte(self, len)
	assert(len==nil, "incorrect call to byte *number) reader")
	return self:read('bytestr'):byte(1)
end
function ParseTable.bytestr(self, len)
	len=len or 1
	assert(len>0, "must read at least one byte!")
	local bytes = self.content:sub(self.ptr, self.ptr+len-1)
	self.ptr = self.ptr + len
	return bytes
end

function ParseTable.boolean(self)
	return (self:read('byte')~=0)
end

function ParseTable.string(self)
	local bytes = self:read('bytestr', self.config.sizes.size_t)
	local len, result, pos = 0
	local inc, start, finish = 1, 1, #bytes
	if self.config.endianness then
		start = finish; finish=1; inc=-1
	end
	for i = start, finish, inc do
		len = len * 256 + bytes:byte(i)
	end
	--print("reading " .. tostring(len) .. " bytes!")

	if len==0 then return '' end
	result = self:read('bytestr', len)
	if result:byte(len) == 0 then result = result:sub(1, -2) end --crop off the null terminator
	--print("ReadString: type->", type(result), result, result:byte(len), result:byte(len-1))
	return result
end

function ParseTable.integer(self)
	local bytes = self:read('bytestr', self.config.sizes.int)
	local result, inc, start, finish = 0, 1, 1, #bytes
	if self.config.endianness then
		start = finish; finish=1; inc=-1
	end
	for i=start, finish, inc do
		local val = bytes:byte(i)
		result = result * 256 + val
	end
	return result
end

local function tobinary(n, bits)
		local binary = { ["0"]="0000", ["1"]="0001", ["2"]="0010", ["3"]="0011", ["4"]="0100", ["5"]="0101", ["6"]="0110", ["7"]="0111", ["8"]="1000", ["9"]="1001", ["a"]="1010", ["b"]="1011", ["c"]="1100", ["d"]="1101", ["e"]="1110", ["f"]="1111", }
		bits = math.floor(bits / 4)
		local str = string.format("%0"..bits.."x",n)
		str = str:gsub(".",function(v) return binary[v] end)
		return str
		--return str:match("^[0]*(%d*)$")
end

function ParseTable.number(self)
	-- Double precision floating-point format:
	-- http://en.wikipedia.org/wiki/Double_precision_floating-point_format
	-- http://babbage.cs.qc.edu/IEEE-754/Decimal.html

	-- some helper functions...
	function binFractionToDec(mantissa)
		local result = 0
		for i = 1,#mantissa do
			if mantissa.sub(i,i)=='1' then result = result + 1 / math.pow(2, i) end
		end
		return result
	end

	local bytes = self:read('bytestr', self.config.sizes.number)
	local data = ''

	local inc, start, finish = 1, 1, #bytes
	if self.config.endianness then
		start = finish; finish=1; inc=-1
	end
	--print("converting number!", start, finish, bytes, self.config.endianness)
	for i= start, finish, inc do
		local str = tobinary(bytes:byte(i),8)
		--print("converted:", bytes:byte(i), str)
		data = data .. str
	end
	local sign = tonumber(data:sub(1,1))
	local exponent = tonumber( data:sub(2,12), 2)
	local mantissa = binFractionToDec( data:sub(13) )
	--[[
	print(data)
	print(data:sub(1,1))
	print(data:sub(2,13))
	print(data:sub(13))
	print(sign, exponent, mantissa)
	--]]
	return math.pow(-1, sign) * (1 + mantissa) * math.pow(2, exponent-1023)
end

 ParseTable["nil"]=function(self)
	return nil
end

function ParseTable.instruction(self)
	return self:read('bytestr', self.config.sizes.instruction)
end

function ParseTable.constant(self)
	local type = self:read('byte')
	--print("Constant", type)
	return self:read( self.types[type])
end

ParseTable["local"] = function(self)
	return {
		varname=self:read('string'),
		startpc=self:read('integer'),
		endpc=self:read('integer')
	}
end

function ParseTable.chunk(self)
	local result = {
		sourceName=self:read('string'),
		lineDefined=self:read('integer'),
		lastLineDefined=self:read('integer'),
		upvalueCount=self:read('byte'),
		paramCount=self:read('byte'),
		is_vararg=self:read('byte'),
		maxStackSize=self:read('byte'),
		instructions= self:parseInstructions( self:readList('instruction')),
		constants = self:readList('constant'),
		functions = self:readList('function'),
		linePositions=self:readList('integer'),
		locals = self:readList('local'),
		upvalues = self:readList('string')
	}

	if self.runConfig.stripDebugging then
		result.linePositions = nil
		result.locals = nil
		result.upvalues = nil
	end
	return result
end

ParseTable["function"] = ParseTable.chunk

function Parser:parseInstructions(instructions)
	local tmpresult = {}
	for _,ins in ipairs(instructions) do
		local v = self:parseInstruction(ins)
		table.insert(tmpresult, v)
	end
	local result = {} -- now we need to flatten out the result table
	for _,list in ipairs(tmpresult) do
		for _,v in ipairs(list) do
			table.insert(result, v)
		end
	end
	return result
end

local function nop()
end
local function bitops(str,shift, nbits) 
	return tonumber(str:sub(-shift-nbits,-shift-1), 2)
end
--local function iAsBx(result, str)result[3] = tonumber(str:sub(1,-15),2)-17 end
local function iAsBx(result, str)result[3] = bitops(str,14,-15) - 0x1ffff end
local function iABx(result, str) result[3] = bitops(str,14,14) end
local InsTable = {
	[1]=iABx, --loadk
	[5]=iABx, --getglobal
	[7]=iABx, --setglobal
	[36]=iABx, --closure

	[22]=iAsBx, --jmp
	[31]=iAsBx, --forloop
	[32]=iAsBx, --forprep
	default=function(result, str)  
		result[3] = bitops(str,23,9)
		result[4] = bitops(str,14,9)
	end
}
function Parser:parseInstruction(ins)
	local result = {0,0,0,0}

	local inc, start, finish = 1, 1, #ins
	if self.config.endianness then
		start=finish; finish=1; inc=-1
	end
	local str = ''
	for i = start, finish, inc do
		str = str .. tobinary( ins:byte(i),8)
	end
	result[1] = bitops(str,0,6)
	result[2] = bitops(str,6,8)
	-- apply instruction specific formatting...
	local func = InsTable[result[1]] or InsTable["default"]
	func(result, str)
	if not self.runConfig.useInstructionObjects then return result end
	return {{op=result[1],
		A=result[2],
		B=result[3],
		C=result[4]
	}}
end

-- -*- coding: utf-8 -*-
--
-- Simple JSON encoding and decoding in pure Lua.
--
-- Copyright 2010-2014 Jeffrey Friedl
-- http://regex.info/blog/
--
-- Latest version: http://regex.info/blog/lua/json
--
-- This code is released under a Creative Commons CC-BY "Attribution" License:
-- http://creativecommons.org/licenses/by/3.0/deed.en_US
--
-- It can be used for any purpose so long as the copyright notice above,
-- the web-page links above, and the 'AUTHOR_NOTE' string below are
-- maintained. Enjoy.
--
local VERSION = 20140920.13  -- version history at end of file
local AUTHOR_NOTE = "-[ JSON.lua package by Jeffrey Friedl (http://regex.info/blog/lua/json) version 20140920.13 ]-"

--
-- The 'AUTHOR_NOTE' variable exists so that information about the source
-- of the package is maintained even in compiled versions. It's included in
-- OBJDEF mostly to quiet warnings about unused variables.
--
local OBJDEF = {
   VERSION      = VERSION,
   AUTHOR_NOTE  = AUTHOR_NOTE,
}


--
-- Simple JSON encoding and decoding in pure Lua. (modified to remove decode
-- routines and removed version history
-- http://www.json.org/

function OBJDEF:onEncodeError(message, etc)
   if etc ~= nil then
      message = message .. " (" .. OBJDEF:encode(etc) .. ")"
   end

   if self.assert then
      self.assert(false, message)
   else
      assert(false, message)
   end
end


local function backslash_replacement_function(c)
   if c == "\n" then
      return "\\n"
   elseif c == "\r" then
      return "\\r"
   elseif c == "\t" then
      return "\\t"
   elseif c == "\b" then
      return "\\b"
   elseif c == "\f" then
      return "\\f"
   elseif c == '"' then
      return '\\"'
   elseif c == '\\' then
      return '\\\\'
   else
      return string.format("\\u%04x", c:byte())
   end
end

local chars_to_be_escaped_in_JSON_string
   = '['
   ..    '"'    -- class sub-pattern to match a double quote
   ..    '%\\'  -- class sub-pattern to match a backslash
   ..    '%z'   -- class sub-pattern to match a null
   ..    '\001' .. '-' .. '\031' -- class sub-pattern to match control characters
   .. ']'

local function json_string_literal(value)
   local newval = value:gsub(chars_to_be_escaped_in_JSON_string, backslash_replacement_function)
   return '"' .. newval .. '"'
end

local function object_or_array(self, T, etc)
   --
   -- We need to inspect all the keys... if there are any strings, we'll convert to a JSON
   -- object. If there are only numbers, it's a JSON array.
   --
   -- If we'll be converting to a JSON object, we'll want to sort the keys so that the
   -- end result is deterministic.
   --
   local string_keys = { }
   local number_keys = { }
   local number_keys_must_be_strings = false
   local maximum_number_key

   for key in pairs(T) do
      if type(key) == 'string' then
         table.insert(string_keys, key)
      elseif type(key) == 'number' then
         table.insert(number_keys, key)
         if key <= 0 or key >= math.huge then
            number_keys_must_be_strings = true
         elseif not maximum_number_key or key > maximum_number_key then
            maximum_number_key = key
         end
      else
         self:onEncodeError("can't encode table with a key of type " .. type(key), etc)
      end
   end

   if #string_keys == 0 and not number_keys_must_be_strings then
      --
      -- An empty table, or a numeric-only array
      --
      if #number_keys > 0 then
         return nil, maximum_number_key -- an array
      elseif tostring(T) == "JSON array" then
         return nil
      elseif tostring(T) == "JSON object" then
         return { }
      else
         -- have to guess, so we'll pick array, since empty arrays are likely more common than empty objects
         return nil
      end
   end

   table.sort(string_keys)

   local map
   if #number_keys > 0 then
      --
      -- If we're here then we have either mixed string/number keys, or numbers inappropriate for a JSON array
      -- It's not ideal, but we'll turn the numbers into strings so that we can at least create a JSON object.
      --

      if self.noKeyConversion then
         self:onEncodeError("a table with both numeric and string keys could be an object or array; aborting", etc)
      end

      --
      -- Have to make a shallow copy of the source table so we can remap the numeric keys to be strings
      --
      map = { }
      for key, val in pairs(T) do
         map[key] = val
      end

      table.sort(number_keys)

      --
      -- Throw numeric keys in there as strings
      --
      for _, number_key in ipairs(number_keys) do
         local string_key = tostring(number_key)
         if map[string_key] == nil then
            table.insert(string_keys , string_key)
            map[string_key] = T[number_key]
         else
            self:onEncodeError("conflict converting table with mixed-type keys into a JSON object: key " .. number_key .. " exists both as a string and a number.", etc)
         end
      end
   end

   return string_keys, nil, map
end

--
-- Encode
--
local encode_value -- must predeclare because it calls itself
function encode_value(self, value, parents, etc, indent) -- non-nil indent means pretty-printing

   if value == nil then
      return 'null'

   elseif type(value) == 'string' then
      return json_string_literal(value)

   elseif type(value) == 'number' then
      if value ~= value then
         --
         -- NaN (Not a Number).
         -- JSON has no NaN, so we have to fudge the best we can. This should really be a package option.
         --
         return "null"
      elseif value >= math.huge then
         --
         -- Positive infinity. JSON has no INF, so we have to fudge the best we can. This should
         -- really be a package option. Note: at least with some implementations, positive infinity
         -- is both ">= math.huge" and "<= -math.huge", which makes no sense but that's how it is.
         -- Negative infinity is properly "<= -math.huge". So, we must be sure to check the ">="
         -- case first.
         --
         return "1e+9999"
      elseif value <= -math.huge then
         --
         -- Negative infinity.
         -- JSON has no INF, so we have to fudge the best we can. This should really be a package option.
         --
         return "-1e+9999"
      else
         return tostring(value)
      end

   elseif type(value) == 'boolean' then
      return tostring(value)

   elseif type(value) ~= 'table' then
      self:onEncodeError("can't convert " .. type(value) .. " to JSON", etc)

   else
      --
      -- A table to be converted to either a JSON object or array.
      --
      local T = value

      if parents[T] then
         self:onEncodeError("table " .. tostring(T) .. " is a child of itself", etc)
      else
         parents[T] = true
      end

      local result_value

      local object_keys, maximum_number_key, map = object_or_array(self, T, etc)
      if maximum_number_key then
         --
         -- An array...
         --
         local ITEMS = { }
         for i = 1, maximum_number_key do
            table.insert(ITEMS, encode_value(self, T[i], parents, etc, indent))
         end

         if indent then
            result_value = "[ " .. table.concat(ITEMS, ", ") .. " ]"
         else
            result_value = "[" .. table.concat(ITEMS, ",") .. "]"
         end

      elseif object_keys then
         --
         -- An object
         --
         local TT = map or T

         if indent then

            local KEYS = { }
            local max_key_length = 0
            for _, key in ipairs(object_keys) do
               local encoded = encode_value(self, tostring(key), parents, etc, "")
               max_key_length = math.max(max_key_length, #encoded)
               table.insert(KEYS, encoded)
            end
            local key_indent = indent .. "    "
            local subtable_indent = indent .. string.rep(" ", max_key_length + 2 + 4)
            local FORMAT = "%s%" .. string.format("%d", max_key_length) .. "s: %s"

            local COMBINED_PARTS = { }
            for i, key in ipairs(object_keys) do
               local encoded_val = encode_value(self, TT[key], parents, etc, subtable_indent)
               table.insert(COMBINED_PARTS, string.format(FORMAT, key_indent, KEYS[i], encoded_val))
            end
            result_value = "{\n" .. table.concat(COMBINED_PARTS, ",\n") .. "\n" .. indent .. "}"

         else

            local PARTS = { }
            for _, key in ipairs(object_keys) do
               local encoded_val = encode_value(self, TT[key],       parents, etc, indent)
               local encoded_key = encode_value(self, tostring(key), parents, etc, indent)
               table.insert(PARTS, string.format("%s:%s", encoded_key, encoded_val))
            end
            result_value = "{" .. table.concat(PARTS, ",") .. "}"

         end
      else
         --
         -- An empty array/object... we'll treat it as an array, though it should really be an option
         --
         result_value = "[]"
      end

      parents[T] = false
      return result_value
   end
end


function OBJDEF:encode(value, etc)
   if type(self) ~= 'table' or self.__index ~= OBJDEF then
      OBJDEF:onEncodeError("JSON:encode must be called in method format", etc)
   end
   return encode_value(self, value, {}, etc, nil)
end

function OBJDEF:encode_pretty(value, etc)
   if type(self) ~= 'table' or self.__index ~= OBJDEF then
      OBJDEF:onEncodeError("JSON:encode_pretty must be called in method format", etc)
   end
   return encode_value(self, value, {}, etc, "")
end

function OBJDEF.__tostring()
   return "JSON encode/decode package"
end

OBJDEF.__index = OBJDEF

function OBJDEF:new(args)
   local new = { }

   if args then
      for key, val in pairs(args) do
         new[key] = val
      end
   end

   return setmetatable(new, OBJDEF)
end
local json = OBJDEF:new()

return Parser
