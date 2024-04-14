# xmllpegparser

`xmllpegparser` is a fast XML parser who uses [`LPeg`](http://www.inf.puc-rio.br/~roberto/lpeg) library.

<!-- summary -->
1. [Installation](#installation)
2. [Test](#test)
3. [xmllpegparser API](#xmllpegparser-api)
    1. [Parsing](#parsing)
    2. [Entity](#entity)
    3. [Parser](#parser)
    4. [Utility](#utility)
    5. [Document structure (default parser)](#document-structure-default-parser)
    6. [Parser structure](#parser-structure)
    7. [Visitor structure](#visitor-structure)
    8. [Default parser limitations](#default-parser-limitations)
5. [Licence](#licence)
<!-- /summary -->


## Installation

```bash
luarocks install --local https://raw.githubusercontent.com/jonathanpoelen/lua-xmllpegparser/master/xmllpegparser-2.2-0.rockspec

# or in your local directory lua-xmllpegparser

luarocks make --local xmllpegparser-2.2-0.rockspec
```

## Test

Run `./example.lua`.

```
./example.lua xmlfile [replaceentities]
```

`replaceentities` = anything, only to enable replacement of entities.


## xmllpegparser API

### Parsing

- `parse(xmlstring[, visitorOrsubEntities[, visitorInitArgs...]])`:\
Returns a tuple `document table, (string error or nil)` (see `visitor.finish`).\
If `subEntities` is `true`, the entities are replaced and a `tentity` member is added to the document `table`.
- `parseFile(filename[, visitorOrsubEntities[, visitorInitArgs...]])`:\
Returns a tuple `document table, error file or error document`.

### Entity

- `defaultEntitiyTable()`:\
Returns the default entity table (`{ quot='"', ... }`).
- `createEntityTable(docEntities[, resultEntities])`:\
Creates an entity table from the document entity table. Return `resultEntities`.
- `mkReplaceEntities(entityTable_or_func)`:\
Returns an LPeg expression that can replace entities
- `replaceEntities(s, entityTable_or_func)`:\
Returns a `string`.

### Parsers

- `parser(visitor[, safeVisitor: bool])`:\
Returns a parser.
If all visitor functions return `nil` (excepted `accuattr`, `init` and `finish`), then `safeVisitor` may be `true` and the parser will optimize the visitor's calls.
- `lazyParser(visitorCreator)`:\
Returns a parser.\
`parser(visitorCreator())` is used on the first call of `myparser.parse(...)`.
- `mkVisitor(evalEntities: bool, defaultEntities: table | function | nil, withoutPosition)`:\
If `not defaultEntities` and `evalEntities` then `defaultEntities = defaultEntityTable()`.\
If `withoutPosition`, then `pos` parameter does not exist for the visitor functions except for `finish`.
- `treeParser`:\
The default parser used by `parse(str, false)`
- `treeParserWithReplacedEntities`:\
The default parser used by `parse(str, true)`
- `treeParserWithoutPos`:\
Parser without `pos` parameter
- `treeParserWithoutPosWithReplacedEntities`:\
Parser without `pos` parameter

### Global parser options

- `enableWithoutPosParser([bool])`:\
Enable default parser with `treeParserWithoutPos*` version.\
`enableParserWithoutPos(false)` is same to `setDefaultParsers()`.\
Returns the previous parsers.
- `setDefaultParsers(parser, parserWithReplacedEntities | bool | nil)`:\
If `parserWithReplacedEntities == true`, then `parserWithReplacedEntities = p`.\
`nil` or `false` value restore the default parser.\
Returns the previous parsers.

### Utility

- `toString(doc: table, indentationText: nil | string, params: nil | table)`:\
  - `indentationText` corresponds to the text used at each indentation level. If `nil`, there is no formatting.
  - `params` is table with
    - `shortEmptyElements: bool = true`: empty tag are self-closed or not.
    - `stableAttributes: bool | function = true`: If `true`, attribute are sorted by name. If a function, it takes the attribute table and should return an iterator function that gives the attribute name and its value.
    - `inlineTextLengthMax: number = 9999999`: a node that contains only one text is formatted on one line. When the text exceeds this value, it is indented.
    - `escape: table`: table of `function(string):string`
      - `attr`: text in double quote
      - `text`: text node
      - `cdata`: text between `<![CDATA[` and `]]>`
      - `comment`: text between `<!--` and `-->`
- `escapeFunctions(escapeAmp: bool = false)`:\
  Utility function for `params.escape` parameter of `toString`
  - `escapeAmp`: escape `&` char in text and attribute
- `escapeComment(string):string`: replace `--` with `—`
- `escapeAttribute(string):string`: replace `<` with `&lt;` and `"` with `&quot;`
- `escapeAttributeAndAmp(string):string`: like `escapeAttribute` + replace `&` with `&amp;`
- `escapeCDATA(string):string`: replace `]]>` with `]]>]]><![CDATA[`
- `escapeText(string):string`: replace `<` with `&lt;`
- `escapeTextAndAmp(string):string` replace `<` with `&lt;` and `&` with `&amp;`


### Document structure (default parser)

```lua
-- pos member = index of string
document = {
  children = {
    { pos=number, parent=table or nil, text=string[, cdata=true] } or
    { pos=number, parent=table or nil, tag=string, attrs={ { name=string, value=string }, ... }, children={ ... } },
    ...
  },
  bad = { children={ ... } } -- when a closed node has no match
  preprocessor = { { pos=number, tag=string, attrs={ { name=string, value=string }, ... } },
  doctype = { pos=number, name=string, ident=string or nil, pubident=string or nil, dtd=string or nil }, -- if there is a doctype
  error = string, -- if error
  lastpos = number, -- last known position of parse()
  entities = { { pos=number, name=string, value=string }, ... },
  tentities = { name=value, ... } -- only if subEntities = true
}
```

### Parser structure

```lua
{
  parse = function(xmlstring, visitorInitArgs...) ... end,
  parseFile = function(filename, visitorInitArgs...) ... end,
  __call = function(xmlstring, visitorInitArgs...) ... end,
}
```

### Visitor structure

Each member is optionnal.

```lua
{
  withPos = bool -- indicates if pos parameter exists in function parameter (except `finish`)
  init = function(...), -- called before parsing, returns the position of the beginning of match or nil
  finish = function(err, pos, xmlstring), -- called after parsing, returns (doc, err) or nil
  proc = function(pos, name, attrs), -- for `<?...?>`
  entity = function(pos, name, value),
  doctype = function(pos, name, ident, pubident, dtd), -- called after all entity()
  accuattr = function(table, name, value), -- `table` is an accumulator that will be transmitted to tag.attrs. Set to `false` for disable this function.
                                           -- If `nil` and `tag` is `not nil`, a default accumalator is used.
                                           -- If `false`, the accumulator is disabled.
                                           -- (`tag(pos, name, accuattr(accuattr({}, attr1, value1), attr2, value2)`)
  tag = function(pos, name, attrs), -- for a new tag (`<a>` or `<a/>`)
  open = function(), -- only for a open node (`<a>` not `<a/>`), called after `tag`.
  close = function(name),
  text = function(pos, text),
  cdata = function(pos, text), -- or `text` if nil
  comment = function(str)
}
```

### Default parser limitations

- Non-validating
- No DTD support
- Ignore processing instructions


## Licence

[MIT license](LICENSE)


<!-- https://github.com/jonathanpoelen/lua-xmllpegparser -->
