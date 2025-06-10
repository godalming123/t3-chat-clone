from options import nil
from asyncdispatch import nil
from macros import strVal, `[]`, kind
from httpx import nil

func flexStyle(nodeName: string): string =
  " style=\"display:flex;gap:0.5rem;" & (if nodeName == "column": "flex-direction:column;" else: "") & "\""

func elemProps(node: NimNode): ((string, string), string) =
  case node.kind
  of macros.nnkCall:
    var elementContents = ""
    for i in 1..macros.len(node)-1:
      var ((elemName, elemProps), elemContents) = elemProps(node[i])
      if elemName == "":
        elementContents &= elemContents
      else:
        elementContents &= "<" & elemName & elemProps & ">" & elemContents & "</" & elemName & ">"
    return (
      (case node[0].strVal
      of "row", "column":
        ("div", flexStyle(node[0].strVal))
      of "text":
        ("span", "")
      else:
        (node[0].strVal, "")),
      elementContents,
    )
  of macros.nnkStrLit:
    return (("", ""), node.strVal)
  else:
    macros.error("Expected `nnkCall` or `nnkStrLit`, but got " & $node.kind, node)

macro page*(contents: untyped): untyped =
  var ((_, props), content) = elemProps(contents)
  return macros.newLit("<html><body" & props & ">" & content & "</html></body>")

const homePage = page(
  row(
    column(
      button("chat 1"),
      button("chat 2"),
      button("chat 3"),
      button("chat 4"),
    ),
    h1("hello world")
  )
)

proc onRequest(req: httpx.Request): asyncdispatch.Future[void] =
  if httpx.httpMethod(req) == options.some(httpx.HttpGet):
    if httpx.path(req) == options.some("/"):
      httpx.send(req, homePage)
    else:
      httpx.send(req, httpx.Http404)

httpx.run(onRequest)
