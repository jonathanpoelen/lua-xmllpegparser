-- from https://github.com/jonathanpoelen/lua-xmllpegparser

local lpeg = require'lpeg'
local S = lpeg.S
local C = lpeg.C
local R = lpeg.R
local Ct = lpeg.Ct
local Cg = lpeg.Cg
local Cf = lpeg.Cf
local Cs = lpeg.Cs
local P = lpeg.P
local I = lpeg.Cp()
local Cc = lpeg.Cc
local Ce = Cc()

local Space = S' \n\t'
local Space0 = Space^0
local Space1 = Space^1
local  String = (S"'" *   (1-S"'")^0  * S"'") + (S'"' *   (1-S'"')^0  * S'"')
local CString = (S"'" * C((1-S"'")^0) * S"'") + (S'"' * C((1-S'"')^0) * S'"')
local  Name = ((R('az','AZ') + S'_') * (R('az','AZ') + S'_-:' + R'09')^0)
local CName = C(Name)
local  Attr =   ( Name * Space0 * '=' * Space0 *  String )
local CAttr = Cg(CName * Space0 * '=' * Space0 * CString)
local  XMLComment = '<!--' *  (1-P'-->')^0 * '-->'
local CXMLComment = '<!--' * C(1-P'-->')^0 * '-->'
local  Entity =   ('<!ENTITY' * Space1 *  Name * Space1 *  String * Space0 * '>')
local CEntity = Cg('<!ENTITY' * Space1 * CName * Space1 * CString * Space0 * '>')

local noop = function()end

local mt = {__call = function(_, ...) return _.parse(...) end}

local addI = function(x) return I * x end
local ident = function(x) return x end

-- force a no captured value with a query function
local unsafeCall = function(patt, func)
  return patt / function(...) func(...) end
end

local safeCall = function(patt, func)
  return patt / func
end

local _parser = function(v, safeVisitor)
  local call = safeVisitor == true and safeCall or unsafeCall
  local mark = (v.withpos and addI or ident)

  local Comment = v.comment and call(CXMLComment, v.comment) or XMLComment
  local Comments = Space0 * (Comment * Space0)^0

  local hasAttr = v.accuattr or (v.accuattr ~= false and (v.tag or v.proc))
  local CAttrs = hasAttr and
    Cf(Ct'' * (Space1 * CAttr)^0, v.accuattr or rawset) * Space0
  local Attrs =
              (Space1 *  Attr)^0                        * Space0
  local ProcAttrs = (v.accuattr or (hasAttr and v.proc)) and CAttrs or Attrs
  local TagAttrs  = (v.accuattr or (hasAttr and v.tag )) and CAttrs or Attrs

  local Preproc = v.proc and
    (Comments * call(mark('<?') * CName * ProcAttrs * '?>', v.proc))^0 or
    (Comments *           '<?'  *  Name * ProcAttrs * '?>'         )^0

  local Entities = v.entity and
    (Comments * call(Cg(mark(CEntity)), v.entity))^0 or
    (Comments *               Entity             )^0

  local DoctypeEnt = Space0 * (P'>' + '[' * Entities * Comments * ']' * Space0 * '>')
  local Doctype = v.doctype and
    Comments * (call(mark('<!DOCTYPE') * Space1 * CName * (Space1
      * ( C('SYSTEM') * Space1 * Cc(nil) * CString
        + C(R'AZ'^1) * Space1 * CString * Space1 * CString
      ))^-1 * DoctypeEnt, v.doctype))^-1
    or
    Comments * (          '<!DOCTYPE'  * Space1 *  Name * (Space1
      *     R'AZ'^1  * Space1 * String * (Space1 * String)^-1
       )^-1 * DoctypeEnt)^-1

  local Tag = v.tag and
    '<' * call(mark(CName) * TagAttrs, v.tag) or
    '<' *            Name  * TagAttrs

  local Open = v.open and
    P'>' * call(Ce, v.open) + '/>' or
    P'>'                    + '/>'

  local Close = v.close and
    '</' * call(mark(CName), v.close) * Space0 * '>' or
    '</' *            Name            * Space0 * '>'

  local Text = v.text and
    call(mark(C((Space0 * (1-S" \n\t<")^1)^1)), v.text) or
               ((Space0 * (1-S" \n\t<")^1)^1)

  local Cdata = (v.cdata or v.text) and
    '<![CDATA[' * call(mark(C((1 - P']]>')^0) * ']]>'), v.cdata or v.text) or
    '<![CDATA[' *            ((1 - P']]>')^0) * ']]>'

  local G = Preproc * Doctype * (Space0 * (Tag * Open + Close + Comment + Cdata + Text))^0 * Space0 * I

  local init, finish = (v.init or noop), (v.finish or noop)

  return function(s, ...)
    local err
    local pos = init(...)
    pos = G:match(s, pos)
    if #s >= pos then
      err = 'parse error at position ' .. tostring(pos)
    end

    local doc, verr = finish(err, pos, s)
    return doc, (verr == nil and err or verr)
  end
end


local mkparser = function(pf)
  local p
  p = setmetatable({
    parse     = pf,
    parseFile = function(filename, ...)
      local f, err = io.open(filename)
      if f then
        local content = f:read'*a'
        f:close()
        return p.parse(content, ...), nil
      end
      return f, err
    end,
  }, mt)
  return p
end

--! Create a parser.
--! @param[in] visitor table : see mkVisitor()
--! @param[in] safeVisitor boolean : when true, optimizes the parser.
--!  Should only be used if all visitor functions (except init and finish) return nil
--! @return Parser
--! @code
--! @code
--!   -- all values are optional
--!   visitor = {
--!     withpos = boolean, -- indicates if pos parameter exists in function parameter (except `finish`)
--!     init = function(...), -- called before parsing, returns the position of the beginning of match or nil
--!     finish = function(err, pos, xmlstring), -- called after parsing, returns (doc, err) or nil
--!     proc = function(pos, name, attrs) or function(name, attrs), -- for `<?...?>`
--!     entity = function(entityName, entityValue),
--!     doctype = function(pos, name, ident, pubident, dtd) or function(name, ident, pubident, dtd), -- called after all entity()
--!     accuattr = function(table, entityName, entityValue),
--!         -- `table` is an accumulator that will be transmitted to tag.attrs.
--!         -- Set to `false` for disable this function.
--!         -- If `nil` and `tag` is `not nil`, a default accumalator is used.
--!         -- If `false`, the accumulator is disabled.
--!         -- (`tag(pos, name, accuattr(accuattr({}, attr1, value1), attr2, value2)`)
--!     tag = function(name, attrs), -- for a new tag (`<a>` or `<a/>`)
--!     open = function(), -- only for a open node (`<a>` not `<a/>`), called after `tag`.
--!     close = function(name),
--!     text = function(text),
--!     cdata = function(text), -- or `text` if nil
--!     comment = function(str),
--!   }
--!
--!   parser = {
--!     __call = --[[call parse]]
--!     parse = function(str, --[[visitorInitArgs]]...),
--!     parseFile = function(filename, --[[visitorInitArgs]]...),
--!   }
--! @endcode
local function parser(visitor, safeVisitor)
  return mkparser(_parser(visitor, safeVisitor))
end

--! Returns the default entity table.
--! @return table
local function defaultEntityTable()
  return { quot='"', apos='\'', lt='<', gt='>', amp='&', tab='\t', nbsp=' ', }
end

local DeclEntity = P'&' * C((1-P';')^1) * P';'

--! Returns an LPeg expression that can replace entities.
--! @code
--!   p = mkReplaceEntities(defaultEntityTable())
--!   str = '<b>a &amp; b</b>'
--!   str = p:match(str)
--!   assert(str == '<b>a & b</b>')
--! @endcode
local function mkReplaceEntities(repl)
  return Cs((DeclEntity / repl + 1)^0)
end

--! @param[in] s string
--! @param[in] entities table : with entity name as key and value as replacement
--! @return string
local function replaceEntities(s, entities)
  return s:gsub('&([^;]+);', entities)
end

--! Add entities to resultEntities from the document entity table.
--! Create new table when resultEntities is nil.
--! @param[in] docEntities table
--! @param[in,out] resultEntities table|nil
--! @return resultEntities or a new table when nil
local function createEntityTable(docEntities, resultEntities)
  local entities = resultEntities or defaultEntityTable()
  for _,e in pairs(docEntities) do
    e.value = replaceEntities(e.value, entities)
    entities[e.name] = e.value
  end
  return entities
end

--! Create a visitor.
--! If `not defaultEntities` and `evalEntities` then `defaultEntities = defaultEntityTable()`.\
--! If `withoutPosition`, then `pos` parameter does not exist for the visitor functions except for `finish`.
--! @param[in] evalEntities boolean
--! @param[in] defaultEntities boolean|table|function
--! @param[in] withoutPosition boolean
--! @return visitor table and true for safeVisitor (see parser())
local function mkVisitor(evalEntities, defaultEntities, withoutPosition)
  local root, elem, doc, bad, SubEntity, accuattr, doctype, text, badclose
  local mkDefaultEntities = defaultEntities and (
    type(defaultEntities) == 'table' and function()
      local t = {}
      for k,e in pairs(defaultEntities) do
        t[k] = e
      end
      return t
    end
    or defaultEntities
  ) or defaultEntityTable

  if evalEntities then
    accuattr = function(a,k,v)
      a[k] = SubEntity:match(v)
      return a
    end

    doctype = withoutPosition and function(name, ident, pubident, dtd)
      doc.doctype = {name=name, ident=ident, pubident=pubident, dtd=dtd}
      doc.tentities = createEntityTable(doc.entities, mkDefaultEntities())
      SubEntity = mkReplaceEntities(doc.tentities)
    end or function(pos, name, ident, pubident, dtd)
      doc.doctype = {name=name, ident=ident, pubident=pubident, dtd=dtd, pos=pos}
      doc.tentities = createEntityTable(doc.entities, mkDefaultEntities())
      SubEntity = mkReplaceEntities(doc.tentities)
    end

    text = withoutPosition and function(str)
      elem.children[#elem.children+1] = {parent=elem, text=SubEntity:match(str)}
    end or function(pos, str)
      elem.children[#elem.children+1] = {parent=elem, text=SubEntity:match(str), pos=pos}
    end
  else
    -- accuattr = noop

    doctype = withoutPosition and function(name, ident, pubident, dtd)
      doc.doctype = {name=name, ident=ident, pubident=pubident, dtd=dtd}
    end or function(pos, name, ident, pubident, dtd)
      doc.doctype = {name=name, ident=ident, pubident=pubident, dtd=dtd, pos=pos}
    end

    text = withoutPosition and function(str)
      elem.children[#elem.children+1] = {parent=elem, text=str}
    end or function(pos, str)
      elem.children[#elem.children+1] = {parent=elem, text=str, pos=pos}
    end
  end

  local pushCloseError = function(tagname, pos)
    local errElem = withoutPosition
      and {tag=tagname, children={}}
      or {tag=tagname, children={}, pos=pos-2}
    bad.children[#bad.children+1] = errElem
    badclose = badclose or errElem
    elem = elem or bad
  end

  return {
    withpos=not withoutPosition,
    accuattr=accuattr,
    doctype=doctype,
    text=text,

    cdata = withoutPosition and function(str)
      elem.children[#elem.children+1] = {parent=elem, text=str, cdata=true}
    end or function(pos, str)
      elem.children[#elem.children+1] = {parent=elem, text=str, cdata=true, pos=pos-9}
    end,

    init=function()
      bad = {children={}}
      root = {children={}}
      doc = {preprocessor={}, entities={}, children=root.children}
      elem = root
      badclose = nil
      if evalEntities then
        SubEntity = mkReplaceEntities(mkDefaultEntities())
      end
    end,

    finish=function(err, pos)
      if badclose then
        doc.bad = bad
        err = (err and err .. ' ' or '')
           .. 'No matching opening tag for ' .. tostring(badclose.tag)
           .. (badclose.pos and ' at position ' .. tostring(badclose.pos) or '')
      elseif root ~= elem then
        err = (err and err .. ' ' or '')
           .. 'No matching closing tag for ' .. tostring(elem.tag)
           .. (elem.pos and ' at position ' .. tostring(elem.pos) or '')
      end

      doc.lastpos = pos
      if err then
        doc.error = err
      end
      return doc, err
    end,

    proc=withoutPosition and function(name, attrs)
      doc.preprocessor[#doc.preprocessor+1] = {tag=name, attrs=attrs}
    end or function(pos, name, attrs)
      doc.preprocessor[#doc.preprocessor+1] = {tag=name, attrs=attrs, pos=pos}
    end,

    entity=withoutPosition and function(k, v)
      doc.entities[#doc.entities+1] = {name=k, value=v}
    end or function(pos, k, v)
      doc.entities[#doc.entities+1] = {name=k, value=v, pos=pos}
    end,

    tag=withoutPosition and function(name, attrs)
      elem.children[#elem.children+1] = {tag=name, attrs=attrs, parent=elem, children={}}
    end or function(pos, name, attrs)
      elem.children[#elem.children+1] = {tag=name, attrs=attrs, parent=elem, children={}, pos=pos-1}
    end,

    open=function()
      elem = elem.children[#elem.children]
    end,

    close=withoutPosition and function(tagname)
      local currentTag = elem.tag
      elem = elem.parent
      if elem and currentTag == tagname then
        return
      end
      pushCloseError(tagname)
    end or function(pos, tagname)
      local currentTag = elem.tag
      elem = elem.parent
      if elem and currentTag == tagname then
        return
      end
      pushCloseError(tagname, pos)
    end,
  }, true -- safeVisitor
end

--! Create a parser whose visitor is built on the first call.
--! @param[in] visitorCreator function
--! @return Parser
local function lazyParser(visitorCreator)
  local p
  p = mkparser(function(...)
    p.parse = _parser(visitorCreator())
    return p.parse(...)
  end)
  return p, true
end

--! @{
--! Document structure for default parser:
--! @code
--!   -- pos member = index of string. Only when visitor.withPos == true
--!   document = {
--!     children = {
--!       { pos=number, parent=table or nil, text=string[, cdata=true] } or
--!       { pos=number, parent=table or nil, tag=string, attrs={ { name=string, value=string }, ... }, children={ ... } },
--!       ...
--!     },
--!     bad = { children={ ... } } -- when a closed node has no match
--!     preprocessor = { { pos=number, tag=string, attrs={ { name=string, value=string }, ... } },
--!     doctype = { pos=number, name=string, ident=string, pubident=string or nil, dtd=string or nil }, -- if there is a doctype
--!     error = string, -- if error
--!     lastpos = number, -- last known position of parse()
--!     entities = { { pos=number, name=string, value=string }, ... },
--!     tentities = { name=value, ... } -- only if subEntities = true
--!   }
--! @endcode

-- The default parser used by parse(str, false)
local treeParser = lazyParser(function() return mkVisitor() end)
-- The default parser used by parse(str, true)
local treeParserWithReplacedEntities = lazyParser(function() return mkVisitor(true) end)
-- Parser without `pos` parameter
local treeParserWithoutPos = lazyParser(function() return mkVisitor(nil,nil,true) end)
-- Parser without `pos` parameter
local treeParserWithoutPosWithReplacedEntities = lazyParser(function() return mkVisitor(true,nil,true) end)
--! @}

local _defaultParser, _defaultParserWithReplacedEntities = treeParser, treeParserWithReplacedEntities

--! @param[in] b boolean|nil : when false, sets parsers that do not take a position as default parsers.
--! @return old defaultParser and defaultParserWithReplacedEntities
local function enableWithoutPosParser(b)
  local r1, r2 = _defaultParser, _defaultParserWithReplacedEntities
  if b == nil or b == true then
    _defaultParser, _defaultParserWithReplacedEntities = treeParserWithoutPos, treeParserWithoutPosWithReplacedEntities
  else
    _defaultParser, _defaultParserWithReplacedEntities = treeParser, treeParserWithReplacedEntities
  end
  return r1, r2
end

--! Sets default parsers for without and with entity replacement.
--! @param[in] p table|nil : Use treeParser when p is nil
--! @param[in] pWithReplacedEntities table|boolean|nil :
--! Use treeParserWithReplacedEntities when pWithReplacedEntities is nil
--! @return old defaultParser and defaultParserWithReplacedEntities
local function setDefaultParsers(p, pWithReplacedEntities)
  local r1, r2 = _defaultParser, _defaultParserWithReplacedEntities
  _defaultParser = p or treeParser
  if pWithReplacedEntities == true then
    _defaultParserWithReplacedEntities = _defaultParser
  elseif pWithReplacedEntities == false then
    _defaultParserWithReplacedEntities = treeParserWithReplacedEntities
  else
    _defaultParserWithReplacedEntities = pWithReplacedEntities or treeParserWithReplacedEntities
  end
  return r1, r2
end

--! Returns a parser.
--! @param[in] visitorOrEvalEntities table|bool|nil :
--!   When visitorOrEvalEntities is a boolean or nil,
--!   a default parser is returned (see \c setDefaultParsers()).
--!   Otherwise visitorOrEvalEntities is returned.
--! @return Parser
local getParser = function(visitorOrEvalEntities)
  return (not visitorOrEvalEntities and _defaultParser) or
         (visitorOrEvalEntities == true and _defaultParserWithReplacedEntities) or
         parser(visitorOrEvalEntities)
end

--! Returns a tuple `document table, (string error or nil)`. See `visitor.finish`.
--! @param[in] s string : xml data
--! @param[in,out] visitorOrEvalEntities table|bool|nil : see \c getParser()
--! @param[in,out] ... argument for visitor.init()
--! @return table
local function parse(xmlstring, visitorOrEvalEntities, ...)
  return getParser(visitorOrEvalEntities).parse(xmlstring, ...)
end

--! Return a tuple `document table, error file`.
--! @param filename[in] string
--! @param[in,out] visitorOrEvalEntities table|bool|nil : see \c getParser()
--! @param[in,out] ... argument for visitor.init()
--! @return table
local function parseFile(filename, visitorOrEvalEntities, ...)
  return getParser(visitorOrEvalEntities).parseFile(filename, ...)
end


local function flatAttrCmp(a, b)
  return a[1] < b[1]
end

local tinsert = table.insert
local tremove = table.remove

local function insertAttrs(t, it, escapeAttr)
  for name,value in it do
    tinsert(t, ' ')
    tinsert(t, name)
    tinsert(t, '="')
    tinsert(t, escapeAttr(value))
    tinsert(t, '"')
  end
end

local function toStringComputeIndent(tindent, lvl, indentationText)
  local prefix = tindent[lvl]
  if not prefix then
    prefix = tindent[lvl - 1] .. indentationText
    tindent[lvl] = prefix
  end
  return prefix
end

local function identity(x)
  return x
end

local function escapeComment(s)
  s = s:gsub('--', '—')
  return s
end

local function escapeAttribute(s)
  s = s:gsub('<', '&lt;'):gsub('"', '&quot;')
  return s
end

local function escapeAttributeAndAmp(s)
  s = s:gsub('&', '&amp;'):gsub('<', '&lt;'):gsub('"', '&quot;')
  return s
end

local function escapeCDATA(s)
  s = s:gsub(']]>', ']]>]]><![CDATA[')
  return s
end

local function escapeText(s)
  s = s:gsub('<', '&lt;')
  return s
end

local function escapeTextAndAmp(s)
  s = s:gsub('&', '&amp;'):gsub('<', '&lt;')
  return s
end

--! Returns a table of functions to escape texts
--! @params[in] escapeAmp bool : escape & char in text and attribute
--! @return {
--!   attr = function(string):string,
--!   text = function(string):string,
--!   cdata = function(string):string,
--!   comment = function(string):string,
--! }
local function escapeFunctions(escapeAmp)
  return {
    attr = escapeAmp and escapeAttributeAndAmp or escapeAttribute,
    text = escapeAmp and escapeTextAndAmp or escapeText,
    cdata = escapeCDATA,
    comment = escapeComment,
  }
end

--! Converts a `document table` to string.
--! @param[in] tdoc table : document table
--! @param[in] indentationText nil | string :
--!   Text used to add a level of indentation.
--!   The nil value corresponds to no formatting.
--! @param[in] params nil | table :
--!   Table that corresponds to additional parameters
--!   - \b shortEmptyElements bool = true :
--!     If true, elements that contain no content are emitted as a single self-closed tag,
--!     otherwise they are emitted as a pair of start/end tags.
--!   - \b stableAttributes bool | function = true :
--!     If true, the attributes will be sorted by name, otherwise the order
--!     depends on that of the array (which depends on the interpreter used).
--!     A function can also be passed. It takes the attribute table and should
--!     return an iterator function that gives the attribute name and its value.
--!   - \b inlineTextLengthMax number = 9999999 :
--!     When a node contains only a single text, the node is formatted on a single line.
--!     If the text length exceeds the value specified in \c inlineTextLengthMax,
--!     it will be indented
--!   - \b escapes table : table of function(string):string
--!       - \b attr: text in double quote
--!       - \b text: text node
--!       - \b cdata: text between <![CDATA[ and ]]>
--!       - \b comment: text between <!-- and -->
--! @return string
local function documentToString(tdoc, indentationText, params)
  local escapeFns = params and params.escapes
  -- luacheck: push ignore 431
  local escapeAttr = escapeFns and escapeFns.attr or identity
  local escapeText = escapeFns and escapeFns.text or identity
  local escapeCDATA = escapeFns and escapeFns.cdata or identity
  local escapeComment = escapeFns and escapeFns.comment or identity
  -- luacheck: pop
  local inlineTextLengthMax = params and params.inlineTextLengthMax or 9999999
  local shortEmptyElements = not params or params.shortEmptyElements == nil or params.shortEmptyElements

  local attrIter
  if not params or params.stableAttributes == nil or params.stableAttributes == true then
    attrIter = function(attrs)
      local flatAttrs = {}
      for attr,value in pairs(attrs) do
        tinsert(flatAttrs, {attr,value})
      end

      table.sort(flatAttrs, flatAttrCmp)

      local idx = 0
      return function() -- simplified iterator since used only once
        idx = idx + 1
        local t = flatAttrs[idx]
        if t then
          return t[1], t[2]
        end
      end, flatAttrs, nil
    end
  elseif params.stableAttributes == false then
    attrIter = identity
  else
    attrIter = params.stableAttributes
  end

  local strs = {}

  local proc = tdoc.preprocessor
  if proc then
    for _, e in pairs(proc) do
      tinsert(strs, '<?')
      tinsert(strs, e.tag)
      insertAttrs(strs, attrIter(e.attrs), escapeAttr)
      tinsert(strs, '?>')
    end
  end

  local prefix = indentationText and '\n' or ''
  local tindent = {prefix}

  indentationText = indentationText or ''

  local doctype = tdoc.doctype
  if doctype then
    if proc then
      tinsert(strs, prefix)
    end

    tinsert(strs, '<!DOCTYPE ')
    tinsert(strs, doctype.name or '')
    if doctype.ident then
      tinsert(strs, ' ')
      tinsert(strs, doctype.ident)
      if doctype.ident then
        tinsert(strs, ' "')
        tinsert(strs, doctype.pubident)
        tinsert(strs, '"')
      end
      if doctype.dtd then
        tinsert(strs, ' "')
        tinsert(strs, doctype.dtd)
        tinsert(strs, '"')
      end
    end

    if tdoc.entities or tdoc.tentities then
      local indent = prefix .. indentationText
      tindent[2] = indent

      local addEntity = function(name, value)
        tinsert(strs, indent)
        tinsert(strs, '<!ENTITY ')
        tinsert(strs, name)
        tinsert(strs, ' "')
        tinsert(strs, value)
        tinsert(strs, '">')
      end

      tinsert(strs, '[')
      if tdoc.entities then
        for _,t in pairs(tdoc.entities) do
          addEntity(t.name, t.value)
        end
      else
        for name,value in pairs(tdoc.tentities) do
          addEntity(name, value:gsub('%', '&#37;'))
        end
      end
      tinsert(strs, prefix)
      tinsert(strs, ']')
    end

    tinsert(strs, '>')
  end

  local elems = tdoc.children
  if elems and elems[1] then
    local emptyTable = {}

    local lvl = 1
    local depths = {}

    local i = 1
    local e, e2, tag, children, node

    ::loop::

    e = elems[i]
    tag = e.tag

    -- tag
    if tag then
      tinsert(strs, prefix)
      tinsert(strs, '<')
      tinsert(strs, tag)
      insertAttrs(strs, attrIter(e.attrs), escapeAttr)

      children = e.children or emptyTable

      -- has at least 2 children or a tag as child
      if children[2] or (children[1] and children[1].tag) then
        tinsert(strs, '>')

        tinsert(depths, {elems, i})
        i = 0
        elems = children
        lvl = lvl + 1
        prefix = toStringComputeIndent(tindent, lvl, indentationText)

      -- only has one text as child
      elseif children[1] and children[1].text then
        tinsert(strs, '>')
        e2 = children[1]
        -- CDATA
        if e2.cdata then
          tinsert(strs, toStringComputeIndent(tindent, lvl+1, indentationText))
          tinsert(strs, '<![CDATA[')
          tinsert(strs, escapeCDATA(e2.text))
          tinsert(strs, ']]>')
          tinsert(strs, prefix)
        -- inline text
        elseif #e2.text <= inlineTextLengthMax then
          tinsert(strs, escapeText(e2.text))
        -- text
        else
          tinsert(strs, toStringComputeIndent(tindent, lvl+1, indentationText))
          tinsert(strs, escapeText(e2.text))
          tinsert(strs, prefix)
        end
        tinsert(strs, '</')
        tinsert(strs, tag)
        tinsert(strs, '>')

      -- empty short tag
      elseif shortEmptyElements then
        tinsert(strs, '/>')

      -- empty tag
      else
        tinsert(strs, '></')
        tinsert(strs, tag)
        tinsert(strs, '>')
      end

    -- text
    elseif e.text then
      -- CDATA
      if e.cdata then
        tinsert(strs, prefix)
        tinsert(strs, '<![CDATA[')
        tinsert(strs, escapeCDATA(e.text))
        tinsert(strs, ']]>')
      else
        tinsert(strs, prefix)
        tinsert(strs, escapeText(e.text))
      end

    -- comment
    elseif e.comment then
      tinsert(strs, prefix)
      tinsert(strs, '<!--')
      tinsert(strs, escapeComment(e.comment))
      tinsert(strs, '-->')
    end

    i = i + 1
    e = elems[i]

    -- close parent
    while not e do
      node = tremove(depths)
      if not node then
        return table.concat(strs, '')
      end
      elems = node[1]
      i = node[2]
      lvl = lvl - 1
      prefix = tindent[lvl]
      tinsert(strs, prefix)
      tinsert(strs, '</')
      tinsert(strs, elems[i].tag)
      tinsert(strs, '>')
      i = i + 1
      e = elems[i]
    end

    goto loop
  end

  return table.concat(strs, '')
end

return {
  defaultEntityTable = defaultEntityTable,
  mkReplaceEntities = mkReplaceEntities,
  replaceEntities = replaceEntities,
  createEntityTable = createEntityTable,
  mkVisitor = mkVisitor,
  lazyParser = lazyParser,
  treeParser = treeParser,
  treeParserWithReplacedEntities = treeParserWithReplacedEntities,
  treeParserWithoutPos = treeParserWithoutPos,
  treeParserWithoutPosWithReplacedEntities = treeParserWithoutPosWithReplacedEntities,
  enableWithoutPosParser = enableWithoutPosParser,
  setDefaultParsers = setDefaultParsers,
  parser = parser,
  parse = parse,
  parseFile = parseFile,
  tostring = documentToString,
  escapeFunctions = escapeFunctions,
  escapeComment = escapeComment,
  escapeAttribute = escapeAttribute,
  escapeAttributeAndAmp = escapeAttributeAndAmp,
  escapeCDATA = escapeCDATA,
  escapeText = escapeText,
  escapeTextAndAmp = escapeTextAndAmp,
}
