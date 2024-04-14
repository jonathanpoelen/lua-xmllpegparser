#!/usr/bin/env lua

print_or_error = (#arg ~= 0 and error or print)

function str(t, prefix)
  if not t then
    return ''
  end
  local orderedIndex = {}
  for i in pairs(t) do
    table.insert(orderedIndex, i)
  end
  table.sort(orderedIndex)

  local s, e = '{\n'
  for _, i in pairs(orderedIndex) do
    e = t[i]
    if type(e) == 'table' then
      if i ~= 'parent' then
        e = next(e) and str(e, prefix .. '  ') or '{}'
      else
        e = e.tag
      end
    end
    s = s .. prefix .. '  ' .. i .. ': ' .. tostring(e) .. ',\n'
  end
  return s .. prefix .. '}'
end

r = 0

function printError(a, b, input, err, ierr)
  -- compute the position where the difference begins
  local idiffer
  for i=1,math.min(#a, #b) + 1 do
    if a:byte(i) ~= b:byte(i) then
      idiffer = i - 1
      break
    end
  end

  print_or_error('[FAILURE]\n  '
    .. a:sub(1, idiffer) .. '\x1b[31m' .. a:sub(idiffer+1) .. '\x1b[m'
    .. '\n  ==\n  '
    .. b:sub(1, idiffer) .. '\x1b[31m' .. b:sub(idiffer+1) .. '\x1b[m'
    .. '\n\n with ' .. input
  )

  if err then print('  ' .. err .. '/' .. ierr) end
  print()

  r = r + 1
end

function check(tdoc, err, s, input, ierr, resultError)
  local doc = str(tdoc, '  ')
  if resultError ~= err or s ~= doc then
    printError(s, doc, input, err, ierr)
  end
end

function _eq(parser, s, sxml, replaceEntities, resultError)
  local tdoc, err = parser.parse(sxml, replaceEntities)
  check(tdoc, err, s, sxml, #sxml, resultError)
end

function _feq(parser, s, filename)
  local tdoc, err = parser.parseFile(filename)
  check(tdoc, err, s, 'file ' .. filename, '???', resultError)
end

function _nopos(s)
  return s:gsub('\n%s+pos: %d+,', ''):gsub(' at position %d+', '')
end

function mkEq(eq)
  return function(s, filename_or_sxml, replaceEntities, resultError)
    eq(xmllpegparser, s, filename_or_sxml, replaceEntities, resultError)
    xmllpegparser.enableWithoutPosParser()
    eq(xmllpegparser, _nopos(s), filename_or_sxml, replaceEntities, resultError and _nopos(resultError))
    xmllpegparser.enableWithoutPosParser(false)
  end
end

local eq, feq = mkEq(_eq), mkEq(_feq)


xmllpegparser = require'xmllpegparser'

-- empty file
eq([[{
    children: {},
    entities: {},
    lastpos: 1,
    preprocessor: {},
  }]],
   '')

-- comment
eq([[{
    children: {},
    entities: {},
    lastpos: 17,
    preprocessor: {},
  }]],
   '<!-- comment -->')

-- single text
eq([[{
    children: {
      1: {
        parent: nil,
        pos: 1,
        text: abc,
      },
    },
    entities: {},
    lastpos: 4,
    preprocessor: {},
  }]],
   'abc')

-- single inline tag
eq([[{
    children: {
      1: {
        attrs: {},
        children: {},
        parent: nil,
        pos: 1,
        tag: a,
      },
    },
    entities: {},
    lastpos: 5,
    preprocessor: {},
  }]],
   '<a/>')

-- single tag
eq([[{
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: a,
            pos: 5,
            text: b,
          },
        },
        parent: nil,
        pos: 1,
        tag: a,
      },
    },
    entities: {},
    lastpos: 11,
    preprocessor: {},
  }]],
   '<a> b </a>')

-- CDATA
eq([[{
    children: {
      1: {
        cdata: true,
        parent: nil,
        pos: 1,
        text:  xy &amp; ,
      },
    },
    entities: {},
    lastpos: 23,
    preprocessor: {},
  }]],
   '<![CDATA[ xy &amp; ]]>')

-- CDATA with entity replacement
eq([[{
    children: {
      1: {
        cdata: true,
        parent: nil,
        pos: 1,
        text:  xy &amp; ,
      },
    },
    entities: {},
    lastpos: 23,
    preprocessor: {},
  }]],
   '<![CDATA[ xy &amp; ]]>', true)

eq([[{
    children: {
      1: {
        attrs: {},
        children: {},
        parent: nil,
        pos: 1,
        tag: a,
      },
      2: {
        attrs: {},
        children: {
          1: {
            parent: b,
            pos: 11,
            text: ad,
          },
        },
        parent: nil,
        pos: 8,
        tag: b,
      },
      3: {
        attrs: {},
        children: {},
        parent: nil,
        pos: 17,
        tag: c,
      },
      4: {
        attrs: {},
        children: {
          1: {
            attrs: {},
            children: {
              1: {
                parent: e,
                pos: 27,
                text: ds,
              },
            },
            parent: d,
            pos: 24,
            tag: e,
          },
        },
        parent: nil,
        pos: 21,
        tag: d,
      },
      5: {
        attrs: {},
        children: {
          1: {
            parent: f,
            pos: 40,
            text: a,
          },
          2: {
            attrs: {},
            children: {},
            parent: f,
            pos: 41,
            tag: g,
          },
          3: {
            parent: f,
            pos: 45,
            text: b,
          },
        },
        parent: nil,
        pos: 37,
        tag: f,
      },
    },
    entities: {},
    lastpos: 50,
    preprocessor: {},
  }]],
   '<a></a><b>ad</b><c/><d><e>ds</e></d><f>a<g/>b</f>')

eq([[{
    children: {
      1: {
        attrs: {
          name: value,
        },
        children: {},
        parent: nil,
        pos: 1,
        tag: a,
      },
      2: {
        attrs: {
          name: value,
        },
        children: {},
        parent: nil,
        pos: 18,
        tag: b,
      },
      3: {
        attrs: {
          name: value,
        },
        children: {},
        parent: nil,
        pos: 41,
        tag: c,
      },
      4: {
        attrs: {
          name: value,
          name2: value2,
        },
        children: {},
        parent: nil,
        pos: 60,
        tag: d,
      },
    },
    entities: {},
    lastpos: 93,
    preprocessor: {},
  }]],
   '<a name="value"/><b   name  =  "value"/><c name="value"  /><d name="value"  name2="value2"/>')

eq([[{
    children: {
      1: {
        attrs: {
          name: v>a,
        },
        children: {},
        parent: nil,
        pos: 1,
        tag: a,
      },
      2: {
        parent: nil,
        pos: 16,
        text: > b,
      },
      3: {
        attrs: {
          name: >,
        },
        children: {
          1: {
            parent: c,
            pos: 31,
            text: d,
          },
        },
        parent: nil,
        pos: 19,
        tag: c,
      },
      4: {
        attrs: {
          name: a,
        },
        children: {
          1: {
            parent: e,
            pos: 48,
            text: >f,
          },
        },
        parent: nil,
        pos: 36,
        tag: e,
      },
    },
    entities: {},
    lastpos: 54,
    preprocessor: {},
  }]],
   '<a name="v>a"/>> b<c name=">">d</c><e name="a">>f</e>')

-- entity without replacement
eq([[{
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: a,
            pos: 75,
            text: b,
          },
        },
        parent: nil,
        pos: 72,
        tag: a,
      },
    },
    entities: {
      1: {
        name: e1,
        pos: 29,
        value: fdd>d,
      },
      2: {
        name: e2,
        pos: 53,
        value: a,
      },
    },
    lastpos: 80,
    preprocessor: {},
  }]],
   '<!DOCTYPE l SYSTEM "l.dtd"[ <!ENTITY e1   "fdd>d">  <!ENTITY e2 "a"> ]><a>b</a>')

-- entity with replacement
eq([[{
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: a,
            pos: 75,
            text: fdd>ddsa;,
          },
        },
        parent: nil,
        pos: 72,
        tag: a,
      },
    },
    entities: {
      1: {
        name: e1,
        pos: 29,
        value: fdd>d,
      },
      2: {
        name: e2,
        pos: 53,
        value: a,
      },
    },
    lastpos: 90,
    preprocessor: {},
    tentities: {
      amp: &,
      apos: ',
      e1: fdd>d,
      e2: a,
      gt: >,
      lt: <,
      nbsp:  ,
      quot: ",
      tab: ]] .. '\t' .. [[,
    },
  }]],
   '<!DOCTYPE l SYSTEM "l.dtd" [<!ENTITY e1   "fdd>d">  <!ENTITY e2 "a"> ]><a>&e1;ds&e2;;</a>', true)

-- missing closing tag
eq([[{
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: AA,
            pos: 6,
            text: b,
          },
        },
        parent: nil,
        pos: 1,
        tag: AA,
      },
    },
    entities: {},
    error: No matching closing tag for AA at position 1,
    lastpos: 7,
    preprocessor: {},
  }]],
   '<AA> b', false, 'No matching closing tag for AA at position 1')

-- closing tag does not match
eq([[{
    bad: {
      children: {
        1: {
          children: {},
          pos: 8,
          tag: BB,
        },
      },
    },
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: AA,
            pos: 6,
            text: b,
          },
        },
        parent: nil,
        pos: 1,
        tag: AA,
      },
    },
    entities: {},
    error: No matching opening tag for BB at position 8,
    lastpos: 13,
    preprocessor: {},
  }]],
   '<AA> b </BB>', false, 'No matching opening tag for BB at position 8')

-- closing tag only
eq([[{
    bad: {
      children: {
        1: {
          children: {},
          pos: 1,
          tag: BB,
        },
      },
    },
    children: {},
    entities: {},
    error: No matching opening tag for BB at position 1,
    lastpos: 6,
    preprocessor: {},
  }]],
   '</BB>', false, 'No matching opening tag for BB at position 1')

-- closing tag then tag
eq([[{
    bad: {
      children: {
        1: {
          children: {},
          pos: 1,
          tag: BB,
        },
        2: {
          attrs: {},
          children: {},
          parent: nil,
          pos: 6,
          tag: a,
        },
      },
    },
    children: {},
    entities: {},
    error: No matching opening tag for BB at position 1,
    lastpos: 13,
    preprocessor: {},
  }]],
   '</BB><a></a>', false, 'No matching opening tag for BB at position 1')

-- too many closing tag
eq([[{
    bad: {
      children: {
        1: {
          children: {},
          pos: 12,
          tag: BB,
        },
      },
    },
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            parent: a,
            pos: 5,
            text: b,
          },
        },
        parent: nil,
        pos: 1,
        tag: a,
      },
    },
    entities: {},
    error: No matching opening tag for BB at position 12,
    lastpos: 17,
    preprocessor: {},
  }]],
   '<a> b </a> </BB>', false, 'No matching opening tag for BB at position 12')

feq([[{
    children: {
      1: {
        attrs: {},
        children: {
          1: {
            attrs: {
              attribute: &entity1;,
            },
            children: {
              1: {
                parent: lvl1,
                pos: 185,
                text: something,
              },
            },
            parent: xml,
            pos: 157,
            tag: lvl1,
          },
          2: {
            parent: xml,
            pos: 204,
            text: blah blah,
          },
          3: {
            attrs: {
              attribute: value,
            },
            children: {},
            parent: xml,
            pos: 216,
            tag: lvl1,
          },
          4: {
            attrs: {},
            children: {
              1: {
                attrs: {},
                children: {
                  1: {
                    parent: lvl2,
                    pos: 275,
                    text: something,
                  },
                },
                parent: other,
                pos: 262,
                tag: lvl2,
              },
            },
            parent: xml,
            pos: 250,
            tag: other,
          },
        },
        parent: nil,
        pos: 149,
        tag: xml,
      },
    },
    entities: {
      1: {
        name: entity1,
        pos: 88,
        value: something,
      },
      2: {
        name: entity2,
        pos: 121,
        value: test,
      },
    },
    lastpos: 315,
    preprocessor: {
      1: {
        attrs: {
          encoding: UTF-8,
          version: 1.0,
        },
        pos: 1,
        tag: xml,
      },
    },
  }]],
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
_eq(parser, [[{
    pos: 12,
    tagnames: {
      1: a,
      2: b,
    },
  }]],
  '<a><b/></a>'
)


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

peq([[{
    attrs: {},
    children: {
      1: {
        parent: x,
        pos: 4,
        text: xxx/&y;,
      },
    },
    parent: nil,
    pos: 1,
    tag: x,
  }]],
    '<x>&x;/&y;</x>')

peq([[{
    attrs: {},
    children: {
      1: {
        parent: x,
        pos: 51,
        text: xxx/yyy,
      },
    },
    parent: nil,
    pos: 48,
    tag: x,
  }]],
    '<!DOCTYPE l SYSTEM "l.dtd" [<!ENTITY y "yyy">]><x>&x;/&y;</x>')

peq([[{
    attrs: {},
    children: {},
    parent: nil,
    pos: 20,
    tag: a,
  }]],
    '<!DOCTYPE language><a></a>')

peq([[{
    attrs: {},
    children: {},
    parent: nil,
    pos: 22,
    tag: a,
  }]],
    '<!DOCTYPE language[]><a></a>')

peq([[{
    attrs: {},
    children: {},
    parent: nil,
    pos: 39,
    tag: a,
  }]],
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

doctypeEq([[{
    name: language,
  }]],
  '<!DOCTYPE language>')

doctypeEq([[{
    name: language,
  }]],
  '<!DOCTYPE language[]>')

doctypeEq([[{
    name: language,
  }]],
  '<!DOCTYPE language[] >')

doctypeEq([[{
    cat: SYSTEM,
    name: language,
    path: language.dtd,
  }]],
  '<!DOCTYPE language SYSTEM "language.dtd">')

doctypeEq([[{
    cat: SYSTEM,
    name: language,
    path: language.dtd,
  }]],
  '<!DOCTYPE language SYSTEM "language.dtd"[]>')


tdoc = xmllpegparser.parse([=[
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE something SYSTEM "something.dtd"
[
   <!ENTITY entity1 "something">
   <!ENTITY entity2 "test">
]>
<xml>
  <lvl1 attribute='&entity1;'>something</lvl1>
  <lvl1 attribute='&entity1;'>something bla bla bla &entity2;</lvl1>
  blah blah
  <![CDATA[ bla &entity1; bla ]]>
  <lvl1 attribute="value"></lvl1>
  <other>
    <lvl2>
      something
    </lvl2>
  </other>
</xml>
]=])

function checkToString(tdoc, s, ...)
  local s2 = xmllpegparser.tostring(tdoc, ...)
  if s ~= s2 then
    printError(s:gsub('\n', '\n  '), s2:gsub('\n', '\n  '), 'TODO')
  end
end

sxml1 = '<xml>' ..
  '<lvl1 attribute="&entity1;">something</lvl1>' ..
  '<lvl1 attribute="&entity1;">something bla bla bla &entity2;</lvl1>' ..
  'blah blah' ..
  '<![CDATA[ bla &entity1; bla ]]>' ..
  '<lvl1 attribute="value"/>' ..
  '<other>' ..
      '<lvl2>something</lvl2>' ..
  '</other>' ..
'</xml>'
checkToString({children=tdoc.children}, sxml1)

checkToString(tdoc, '<?xml encoding="UTF-8" version="1.0"?>' .. sxml1)

checkToString(tdoc, [=[
<?xml encoding="UTF-8" version="1.0"?>
<xml>
    <lvl1 attribute="&entity1;">something</lvl1>
    <lvl1 attribute="&entity1;">something bla bla bla &entity2;</lvl1>
    blah blah
    <![CDATA[ bla &entity1; bla ]]>
    <lvl1 attribute="value"/>
    <other>
        <lvl2>something</lvl2>
    </other>
</xml>]=], '    ')

checkToString(tdoc, [=[
<?xml encoding="UTF-8" version="1.0"?>
<xml>
..<lvl1 attribute="&amp;entity1;">something</lvl1>
..<lvl1 attribute="&amp;entity1;">
....something bla bla bla &amp;entity2;
..</lvl1>
..blah blah
..<![CDATA[ bla &entity1; bla ]]>
..<lvl1 attribute="value"></lvl1>
..<other>
....<lvl2>something</lvl2>
..</other>
</xml>]=],
'..', {
  inlineTextLengthMax = 10,
  shortEmptyElements = false,
  escapes = xmllpegparser.escapeFunctions(true),
})


if 0 == r then
  print('No error')
end
os.exit(r)
