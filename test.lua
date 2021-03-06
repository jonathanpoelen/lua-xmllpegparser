#!/usr/bin/env lua

function str(t)
  if not t then
    return ''
  end
  local orderedIndex = {}
  for i in pairs(t) do
    table.insert(orderedIndex, i)
  end
  table.sort(orderedIndex)

  local s, e = '{'
  for k, i in pairs(orderedIndex) do
    e = t[i]
    if type(e) == 'table' then
      e = (i ~= 'parent') and str(e) or e.tag
    end
    s = s .. i .. ':' .. tostring(e) .. ','
  end
  return s .. '}'
end

r = 0

function check(tdoc, err, s, input, ierr)
  local doc = str(tdoc)
  if err or s ~= doc then
    (#arg ~= 0 and error or print)('[FAILURE]\n  ' .. s .. '\n  ==\n  ' .. doc .. '\n with' .. input)
    if err then print('  ' .. err .. '/' .. ierr) end
    r = r + 1
  end
end

function _eq(parser, s, sxml, replaceEntities)
  local tdoc, err = parser.parse(sxml, replaceEntities)
  check(tdoc, err, s, sxml, #sxml)
end

function _feq(parser, s, filename)
  local tdoc, err = parser.parseFile(filename)
  check(tdoc, err, s, 'file ' .. filename, '???')
end

function _nopos(s)
  return s:gsub(',pos:%d+', '')
end

function mkEq(eq)
  return function(s, ...)
    eq(xmllpegparser, s, ...)
    xmllpegparser.enableWithoutPosParser()
    eq(xmllpegparser, _nopos(s), ...)
    xmllpegparser.enableWithoutPosParser(false)
  end
end

local eq, feq = mkEq(_eq), mkEq(_feq)


xmllpegparser = require'xmllpegparser'

eq('{children:{1:{attrs:{},children:{},parent:nil,pos:1,tag:a,},2:{attrs:{},children:{1:{parent:b,pos:11,text:ad,},},parent:nil,pos:8,tag:b,},3:{attrs:{},children:{},parent:nil,pos:17,tag:c,},4:{attrs:{},children:{1:{attrs:{},children:{1:{parent:e,pos:27,text:ds,},},parent:d,pos:24,tag:e,},},parent:nil,pos:21,tag:d,},5:{attrs:{},children:{1:{parent:f,pos:40,text:a,},2:{attrs:{},children:{},parent:f,pos:41,tag:g,},3:{parent:f,pos:45,text:b,},},parent:nil,pos:37,tag:f,},},entities:{},lastpos:50,preprocessor:{},}',
   '<a></a><b>ad</b><c/><d><e>ds</e></d><f>a<g/>b</f>')

eq('{children:{1:{attrs:{name:value,},children:{},parent:nil,pos:1,tag:a,},2:{attrs:{name:value,},children:{},parent:nil,pos:18,tag:b,},3:{attrs:{name:value,},children:{},parent:nil,pos:41,tag:c,},4:{attrs:{name:value,name2:value2,},children:{},parent:nil,pos:60,tag:d,},},entities:{},lastpos:93,preprocessor:{},}',
   '<a name="value"/><b   name  =  "value"/><c name="value"  /><d name="value"  name2="value2"/>')

eq('{children:{1:{attrs:{name:v>a,},children:{},parent:nil,pos:1,tag:a,},2:{parent:nil,pos:16,text:> b,},3:{attrs:{name:>,},children:{1:{parent:c,pos:31,text:d,},},parent:nil,pos:19,tag:c,},4:{attrs:{name:a,},children:{1:{parent:e,pos:48,text:>f,},},parent:nil,pos:36,tag:e,},},entities:{},lastpos:54,preprocessor:{},}',
   '<a name="v>a"/>> b<c name=">">d</c><e name="a">>f</e>')

eq('{children:{1:{attrs:{},children:{1:{parent:a,pos:5,text:b,},},parent:nil,pos:1,tag:a,},},entities:{},lastpos:11,preprocessor:{},}',
   '<a> b </a>')

eq('{children:{1:{attrs:{},children:{1:{parent:a,pos:75,text:b,},},parent:nil,pos:72,tag:a,},},entities:{1:{name:e1,pos:29,value:fdd>d,},2:{name:e2,pos:53,value:a,},},lastpos:80,preprocessor:{},}',
   '<!DOCTYPE l SYSTEM "l.dtd"[ <!ENTITY e1   "fdd>d">  <!ENTITY e2 "a"> ]><a>b</a>')

eq('{children:{1:{attrs:{},children:{1:{parent:a,pos:75,text:fdd>ddsa;,},},parent:nil,pos:72,tag:a,},},entities:{1:{name:e1,pos:29,value:fdd>d,},2:{name:e2,pos:53,value:a,},},lastpos:90,preprocessor:{},tentities:{amp:&,apos:\',e1:fdd>d,e2:a,gt:>,lt:<,nbsp: ,quot:",tab:\t,},}',
   '<!DOCTYPE l SYSTEM "l.dtd" [<!ENTITY e1   "fdd>d">  <!ENTITY e2 "a"> ]><a>&e1;ds&e2;;</a>', true)


feq('{children:{1:{attrs:{},children:{1:{attrs:{attribute:&entity1;,},children:{1:{parent:lvl1,pos:185,text:something,},},parent:xml,pos:157,tag:lvl1,},2:{parent:xml,pos:204,text:blah blah,},3:{attrs:{attribute:value,},children:{},parent:xml,pos:216,tag:lvl1,},4:{attrs:{},children:{1:{attrs:{},children:{1:{parent:lvl2,pos:275,text:something,},},parent:other,pos:262,tag:lvl2,},},parent:xml,pos:250,tag:other,},},parent:nil,pos:149,tag:xml,},},entities:{1:{name:entity1,pos:88,value:something,},2:{name:entity2,pos:121,value:test,},},lastpos:315,preprocessor:{1:{attrs:{encoding:UTF-8,version:1.0,},pos:1,tag:xml,},},}',
   'example.xml')


tags={}
parser = xmllpegparser.parser{
  tag=function(name)
    tags[#tags+1] = name
    return 'dummy' -- must not influence the result
  end,
  finish=function(err, pos)
    return {tagnames=tags, err=err, pos=pos}
  end
}
_eq(parser, '{pos:12,tagnames:{1:a,2:b,},}', '<a><b/></a>')


mkParser = function(...)
  local v = xmllpegparser.mkVisitor(...)
  local f = v.finish
  v.finish = function(...)
    local doc, err = f(...)
    return (doc and doc.children[1] or {}), err
  end
  return xmllpegparser.parser(v)
end

parser1 = mkParser(true, {x='xxx'})
parser2 = mkParser(true, {x='xxx'}, true)

function peq(s, sxml)
  _eq(parser1, s, sxml)
  _eq(parser2, _nopos(s), sxml)
end

peq('{attrs:{},children:{1:{parent:x,pos:4,text:xxx/&y;,},},parent:nil,pos:1,tag:x,}',
    '<x>&x;/&y;</x>')

peq('{attrs:{},children:{1:{parent:x,pos:51,text:xxx/yyy,},},parent:nil,pos:48,tag:x,}',
    '<!DOCTYPE l SYSTEM "l.dtd" [<!ENTITY y "yyy">]><x>&x;/&y;</x>')

peq('{attrs:{},children:{},parent:nil,pos:20,tag:a,}',
    '<!DOCTYPE language><a></a>')

peq('{attrs:{},children:{},parent:nil,pos:22,tag:a,}',
    '<!DOCTYPE language[]><a></a>')

peq('{attrs:{},children:{},parent:nil,pos:39,tag:a,}',
    '<!DOCTYPE language[<!ENTITY y "yyy">]><a></a>')

do
  local d
  local doctypeVisitor = xmllpegparser.parser({
    init=function()
      d = {}
    end,
    finish=function()
      return d
    end,
    doctype=function(name, cat, path)
      d = {name=name, cat=cat, path=path}
    end
  })

  function doctypeEq(s, sxml)
    _eq(doctypeVisitor, s, sxml)
  end
end

doctypeEq('{name:language,}', '<!DOCTYPE language>')
doctypeEq('{name:language,}', '<!DOCTYPE language[]>')
doctypeEq('{name:language,}', '<!DOCTYPE language[] >')
doctypeEq('{cat:SYSTEM,name:language,path:language.dtd,}',
          '<!DOCTYPE language SYSTEM "language.dtd">')
doctypeEq('{cat:SYSTEM,name:language,path:language.dtd,}',
          '<!DOCTYPE language SYSTEM "language.dtd"[]>')

if 0 == r then
  print('No error')
end
os.exit(r)
