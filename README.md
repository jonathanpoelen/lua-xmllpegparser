# xmllpegparser

`xmllpegparser` is a fast XML parser who uses [`LPeg`](http://www.inf.puc-rio.br/~roberto/lpeg) library.

<!-- summary -->
1. [Installation](#installation)
2. [Test](#test)
3. [xmllpegparser API](#xmllpegparser-api)
    1. [Document structure (default parser)](#document-structure-default-parser)
    2. [Parser structure](#parser-structure)
    3. [Visitor structure](#visitor-structure)
    4. [Default parser limitations](#default-parser-limitations)
5. [Licence](#licence)
<!-- /summary -->


## Installation

```bash
luarocks install --local https://raw.githubusercontent.com/jonathanpoelen/lua-xmllpegparser/master/xmllpegparser-2.1-3.rockspec

# or in your local directory lua-xmllpegparser

luarocks make --local xmllpegparser-2.1-3.rockspec
```

## Test

Run `./example.lua`.

```
./example.lua xmlfile [replaceentities]
```

`replaceentities` = anything, only to enable replacement of entities.


## xmllpegparser API

- `xmllpegparser.parse(xmlstring[, visitorOrsubEntities[, visitorInitArgs...]])`:\
Returns a tuple `document table, (string error or nil)` (see `visitor.finish`).\
If `subEntities` is `true`, the entities are replaced and a `tentity` member is added to the document `table`.
- `xmllpegparser.parseFile(filename[, visitorOrsubEntities[, visitorInitArgs...]])`:\
Returns a tuple `document table, error file or error document`.
- `xmllpegparser.defaultEntitiyTable()`:\
Returns the default entity table (` { quot='"', ... }`).
- `xmllpegparser.createEntityTable(docEntities[, resultEntities])`:\
Creates an entity table from the document entity table. Return `resultEntities`.
- `xmllpegparser.mkReplaceEntities(entityTable_or_func)`:\
Returns a lpeg replace entities context: `str = ctx:match(str)`.
- `xmllpegparser.replaceEntities(s, entityTable_or_func)`:\
Returns a `string`.
- `xmllpegparser.parser(visitor[, safeVisitor:bool])`:\
Returns a parser.
If all visitor functions return `nil` (excepted `accuattr`, `init` and `finish`), then `safeVisitor` may be `true` and the parser will optimize the visitor's calls.
- `xmllpegparser.lazyParser(visitorCreator)`:\
Returns a parser.\
`xmllpegparser.parser(visitorCreator())` is used on the first call of `myparser.parse(...)`.
- `xmllpegparser.mkVisitor(evalEntities:bool, defaultEntities:table|function|nil, withoutPosition)`:\
If `not defaultEntities` and `evalEntities` then `defaultEntities = defaultEntityTable`.\
If `withoutPosition`, then `pos` parameter does not exist for the visitor functions except for `finish`.
- `xmllpegparser.treeParser`:\
The default parser used by `xmllpegparser.parse(s, false)`
- `xmllpegparser.treeParserWithReplacedEntities`:\
The default parser used by `xmllpegparser.parse(s, true)`
- `xmllpegparser.treeParserWithoutPos`:\
Parser without `pos` parameter
- `xmllpegparser.treeParserWithoutPosWithReplacedEntities`:\
Parser without `pos` parameter
- `xmllpegparser.enableWithoutPosParser([bool])`:\
Enable default parser with `treeParserWithoutPos*` version.\
`enableParserWithoutPos(false)` is same to `setDefaultParsers()`.\
Returns the previous parsers.
- `xmllpegparser.setDefaultParsers(parser, parserWithReplacedEntities|bool|nil)`:\
If `parserWithReplacedEntities == true`, then `parserWithReplacedEntities = p`.\
`nil` or `false` value restore the default parser.\
Returns the previous parsers.



### Document structure (default parser)

```lua
-- pos member = index of string
document = {
  children = {
    { pos=integer, parent=table or nil, text=string[, cdata=true] } or
    { pos=integer, parent=table or nil, tag=string, attrs={ { name=string, value=string }, ... }, children={ ... } },
    ...
  },
  bad = { children={ ... } } -- if the number of closed nodes is greater than the open nodes. parent always refers to bad
  preprocessor = { { pos=integer, tag=string, attrs={ { name=string, value=string }, ... } },
  error = string, -- if error
  lastpos = numeric, -- last known position of parse()
  entities = { { pos=integer, name=string, value=string }, ... },
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
  proc = function(pos, name, attrs), -- <?...?>
  entity = function(pos, name, value),
  doctype = function(pos, name, cat, path), -- called after all addEntity
  accuattr = function(table, name, value), -- `table` is an accumulator that will be transmitted to tag.attrs. Set to `false` for disable this function.
                                           -- If `nil` and `tag` is `not nil`, a default accumalator is used.
                                           -- If `false`, the accumulator is disabled.
                                           -- (`tag(pos, name, accuattr(accuattr({}, attr1, value1), attr2, value2)`)
  tag = function(pos, name, attrs), -- for a new tag (`<a>` or `<a/>`)
  open = function(), -- only for a open node (`<a>`), called after `tag`.
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
- Ignore DOCTYPE, parse only ENTITY
- If several attributes have the same name (allowed by the standard), only the last is kept.


## Licence

[MIT license](LICENSE)


<!-- https://github.com/jonathanpoelen/lua-xmllpegparser -->
