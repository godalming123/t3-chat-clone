from options import nil
from asyncdispatch import nil
from httpx import nil
from framework import nil

#func flexStyle(nodeName: string): string =
#  " style=\"display:flex;gap:0.5rem;" &
#    (if nodeName == "column": "flex-direction:column;" else: "") & "\""
#
#func htmlElement(props: ((string, string), string)): string =
#  var ((elemName, elemProps), elemContents) = props
#  if elemName == "":
#    return elemContents
#  else:
#    return "<" & elemName & elemProps & ">" & elemContents & "</" & elemName & ">"
#
#func elemProps(node: NimNode): ((string, string), string) =
#  case node.kind
#  of macros.nnkCall:
#    var elementContents = ""
#    var elementProps = ""
#    for i in 1..macros.len(node)-1:
#      case node[i].kind
#      of macros.nnkExprEqExpr:
#        elementProps &= " " & node[i][0].strVal & node[i][1].strVal
#      else:
#        elementContents &= node[i].elemProps().htmlElement()
#    return (
#      (case node[0].strVal
#      of "row", "column":
#        ("div", flexStyle(node[0].strVal) & elementProps)
#      of "text":
#        ("span", elementProps)
#      else:
#        (node[0].strVal, elementProps)),
#      elementContents,
#    )
#  of macros.nnkStrLit:
#    return (("", ""), node.strVal)
#  else:
#    macros.error("Expected `nnkCall` or `nnkStrLit`, but got " & $node.kind, node)

framework.component(Counter):
  count = state(4)
  doubleCount = derived(count * 2)
  button(
    onclick = block:
      count = count + 1, # TODO: Add support for +=
    "Count is $count$, doubled is $doubleCount$",
  )

framework.component(Chat):
  text = state("Loding...")
  onload = """
    const decoder = new TextDecoder("utf-8");
    let response = await fetch("http://127.0.0.1:11434/api/generate", {
      "body": `{"model": "llama3.2", "prompt": "Why is the sky blue?"}`,
      "method": "POST",
      "mode": "cors"
    })
    let reader = response.body.getReader()
    set_text("")
    while (true) {
      let {value, done} = await reader.read()
      let json = decoder.decode(value, {stream: true})
      if (json != "") {
        try {
          set_text(get_text() + JSON.parse(json).response)
        } catch (err) {
          set_text(`tried to parse the json "${json}", but got error "${err.message}"`)
        }
      }
      if (done) {break}
    }
  """
  span("$text$")
  br()
  input(type = "text", placeholder = "Type your message here...")

const homePage = framework.page(Chat)

proc onRequest(req: httpx.Request): asyncdispatch.Future[void] =
  if httpx.httpMethod(req) == options.some(httpx.HttpGet):
    if httpx.path(req) == options.some("/"):
      httpx.send(req, homePage)
    else:
      httpx.send(req, httpx.Http404)

httpx.run(onRequest)
