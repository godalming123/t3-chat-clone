from helpers import matches, repr
from std/sets import `+`, items
from std/tables import `[]`, `[]=`
import macros
from options import map, get
from strutils import join

var functionIndex {.compileTime.} = 0

proc getNextFunctionName(): string =
  result = "f" & $functionIndex
  functionIndex += 1

type DataType = enum
  Int
  Float
  String

type OptionalData = object
  case kind: DataType
  of Int:    intVal:   options.Option[int]
  of Float:  floatVal: options.Option[float]
  of String: strVal:   options.Option[string]

type Data = object
  case kind: DataType
  of Int:    intVal:   int
  of Float:  floatVal: float
  of String: strVal:   string

func toOptional(d: Data): OptionalData =
  case d.kind:
  of Int:    OptionalData(kind: Int, intVal: options.some(d.intVal))
  of Float:  OptionalData(kind: Float, floatVal: options.some(d.floatVal))
  of String: OptionalData(kind: String, strVal: options.some(d.strVal))

func toNonOptional(d: OptionalData): options.Option[Data] =
  case d.kind:
  of Int:
    if options.isSome(d.intVal): return options.some(Data(kind: Int, intVal: options.get(d.intVal)))
  of Float:
    if options.isSome(d.floatVal): return options.some(Data(kind: Float, floatVal: options.get(d.floatVal)))
  of String:
    if options.isSome(d.strVal): return options.some(Data(kind: String, strVal: options.get(d.strVal)))
  return options.none(Data)

func intToString(value: int): string = $value
func floatToString(value: float): string = $value

func optionalDataToOptionalString(data: OptionalData): options.Option[string] =
  return case data.kind
  of Int: options.map(data.intVal, intToString)
  of Float: options.map(data.floatVal, floatToString)
  of String: data.strVal

func data* (data: int):    Data = Data(kind: Int,    intVal:   data)
func data* (data: float):  Data = Data(kind: Float,  floatVal: data)
func data* (data: string): Data = Data(kind: String, strVal:   data)

type VariableInfoKind = enum
  State
  Derived
  Constant
  Arg

type VariableInfo = ref object
  case kind: VariableInfoKind
  of State:
    stateInitialValue: Data
    stateUpdateValueFuncName: string
    stateUpdateValueFuncContents: seq[string]
  of Derived:
    derivedInitialValue: OptionalData
    recalculateValueFuncName: string
    recalculateValueFuncContents: seq[string]
  of Constant:
    value: Data
  of Arg:
    dataKind: DataType
    argUpdateValueFuncName: string
    argUpdateValueFuncContents: seq[string]

func initialValue(varInfo: VariableInfo): OptionalData =
  case varInfo.kind
  of State:
    return toOptional(varInfo.stateInitialValue)
  of Derived:
    return varInfo.derivedInitialValue
  of Constant:
    return toOptional(varInfo.value)
  of Arg:
    return OptionalData(kind: varInfo.dataKind)

type stateUpdateFunctionOutput = object
  name: string
  contents: seq[string]
  hasNewValueArg: bool

func stateUpdateFunction(varInfo: VariableInfo): options.Option[stateUpdateFunctionOutput] =
  case varInfo.kind
  of State:
    return options.some(stateUpdateFunctionOutput(name: varInfo.stateUpdateValueFuncName, contents: varInfo.stateUpdateValueFuncContents, hasNewValueArg: true))
  of Derived:
    return options.some(stateUpdateFunctionOutput(name: varInfo.recalculateValueFuncName, contents: varInfo.recalculateValueFuncContents, hasNewValueArg: false))
  of Constant:
    return options.none(stateUpdateFunctionOutput)
  of Arg:
    return options.some(stateUpdateFunctionOutput(name: varInfo.argUpdateValueFuncName, contents: varInfo.argUpdateValueFuncContents, hasNewValueArg: true))

func checkVariableExists(data: tables.Table[string, VariableInfo], varName: string, codeLocation: NimNode) =
  if not tables.hasKey(data, varName):
    macros.error("Variable called `" & varName & "` does not exist", codeLocation)

type derivedValue = (OptionalData, string, sets.HashSet[string])

template combineDerivedValue(valueA, valueB: derivedValue, outputKind: DataType, initialVal: untyped, jsValue: untyped): derivedValue =
  var initialValue = OptionalData(kind: outputKind)
  var dataForA = toNonOptional(valueA[0])
  var dataForB = toNonOptional(valueB[0])
  if options.isSome(dataForA) and options.isSome(dataForB):
    let a {.inject.} = options.get(dataForA)
    let b {.inject.} = options.get(dataForB)
    initialValue = toOptional(data(initialVal))
  let a {.inject.} = valueA[1]
  let b {.inject.} = valueB[1]
  (initialValue, jsValue, valueA[2] + valueB[2])

proc calculateDerivedValue(data: tables.Table[string, VariableInfo], derivedValue: NimNode): derivedValue =
  if matches(derivedValue, infix(ident(op), nodeA, nodeB)):
    let valueA = calculateDerivedValue(data, nodeA)
    let valueB = calculateDerivedValue(data, nodeB)
    if valueA[0].kind != valueB[0].kind:
      macros.error("Expected arguments to " & op & " to have the same type, but got different types " & $valueA[0].kind & " and " & $valueB[0].kind, derivedValue)
    let ty = valueA[0].kind
    if   op == "+" and ty == Int:   return combineDerivedValue(valueA, valueB, Int, a.intVal+b.intVal, "parseInt("&a&")+parseInt("&b&")")
    elif op == "-" and ty == Int:   return combineDerivedValue(valueA, valueB, Int, a.intVal-b.intVal, "parseInt("&a&")-parseInt("&b&")")
    elif op == "*" and ty == Int:   return combineDerivedValue(valueA, valueB, Int, a.intVal*b.intVal, "parseInt("&a&")*parseInt("&b&")")
    elif op == "/" and ty == Int:   return combineDerivedValue(valueA, valueB, Int, a.intVal/b.intVal, "parseInt("&a&")/parseInt("&b&")")
    elif op == "+" and ty == Float: return combineDerivedValue(valueA, valueB, Float, a.floatVal+b.floatVal, "parseFloat("&a&")+parseFloat("&b&")")
    elif op == "-" and ty == Float: return combineDerivedValue(valueA, valueB, Float, a.floatVal-b.floatVal, "parseFloat("&a&")-parseFloat("&b&")")
    elif op == "*" and ty == Float: return combineDerivedValue(valueA, valueB, Float, a.floatVal*b.floatVal, "parseFloat("&a&")*parseFloat("&b&")")
    elif op == "/" and ty == Float: return combineDerivedValue(valueA, valueB, Float, a.floatVal/b.floatVal, "parseFloat("&a&")/parseFloat("&b&")")
    else:
      macros.error("Unknown operation and type combination: operation `" & op & "` with type: " & $ty & ".", derivedValue[0])
  elif matches(derivedValue, ident(name)):
    checkVariableExists(data, name, derivedValue)
    return (data[name].initialValue, "componentRoot.getAttribute(`state_" & name & "`)", sets.toHashSet([name]))
  elif matches(derivedValue, intLit(value)):
    (OptionalData(kind: Int, intVal: options.some(value)), $value, sets.initHashSet[string]())
  elif matches(derivedValue, par(contents)):
    calculateDerivedValue(data, contents)
  else:
    macros.error("Did not match derived contents, got " & macros.treeRepr(derivedValue), derivedValue)

# Returns true if the update value func changed and false otherwise
func addToUpdateValueFunc(varInfo: var VariableInfo, codeToAdd: string): bool =
  case varInfo.kind
  of State:
    varInfo.stateUpdateValueFuncContents.add(codeToAdd)
  of Derived:
    varInfo.recalculateValueFuncContents.add(codeToAdd)
  of Constant:
    return false
  of Arg:
    varInfo.argUpdateValueFuncContents.add(codeToAdd)
  return true

func formatString(data: var tables.Table[string, VariableInfo], str: string, codeLoc: NimNode, htmlLoc: string): string =
  var i = 0
  var stringDependencies = newSeq[string]()
  var jsToGenerateString = ""
  while i < len(str):
    case str[i]
    of '\\':
      i += 1
      if str[i] == '\\':
        result &= "\\"
        jsToGenerateString &= "\\"
    of '$':
      i += 1
      let start = i
      while true:
        if i >= len(str):
          macros.error("Unterminated format", codeLoc)
        elif str[i] == '$':
          break
        i += 1
      let varName = str[start..<i]
      checkVariableExists(data, varName, codeLoc)
      result &= data[varName].initialValue().optionalDataToOptionalString().get("LOADING")
      jsToGenerateString &= "${componentRoot.getAttribute(`state_" & varName & "`)}"
      stringDependencies.add(varName)
    else:
      result &= str[i]
      jsToGenerateString &= str[i]
    i += 1
  for dep in stringDependencies:
    discard data[dep].addToUpdateValueFunc(htmlLoc & "=`" & jsToGenerateString & "`")

type Component* = object
  cssClassesRequired: sets.HashSet[string]
  initialHtml: string
  componentJs: string
  jsFromOtherComponentsRequired: sets.HashSet[string]
  # Set to a blank string if the component does not have an onload function
  # The function body is stored within `componentJs`
  onLoadFunctionName: string

var components {.compileTime.} = tables.initTable[string, Component]()

func repr(c: Component): string =
  var cssClassesRequired = newSeq[string](sets.len(c.cssClassesRequired))
  var i = 0
  for class in sets.items(c.cssClassesRequired):
    cssClassesRequired[i] = class
    i += 1
  return "Component(\n  cssClassesRequired: " & $cssClassesRequired &
    "\n  initialHtml: " & repr(c.initialHtml) &
    "\n  componentJs: " & repr(c.componentJs) & "\n)"

proc getInitialHtml(
  data: var tables.Table[string, VariableInfo],
  jsFromOtherComponentsRequired: var sets.HashSet[string],
  cssClassesRequired: var sets.HashSet[string],
  elements: openArray[NimNode],
  htmlLocation: string,
  nestedLayersAfterComponentRoot: int,
  # Key is the components name, value is a sequence of the html locations where that component is used
  # Does not include components that are used indirectly
  otherComponentsUsed: var tables.Table[string, seq[string]],
): string =
  # TODO: Support adding variables to the markup without formatted strings

  assert(len(elements) > 0)

  if len(elements) == 1 and matches(elements[0], strLit(text)):
    # TODO: Support setting `innertext`
    return formatString(data, text, elements[0], htmlLocation & ".innerHTML")

  for i in 0..<elements.len:
    if matches(elements[i], call(ident(elemName), ...args)):
      if elemName.len >= 1 and 'A' <= elemName[0] and elemName[0] <= 'Z':
        helpers.errorAssert(tables.hasKey(components, elemName), "Component called `" & elemName & "` does not exist", elements[i][0])
        if not tables.hasKey(otherComponentsUsed, elemName):
          otherComponentsUsed[elemName] = @[]
        otherComponentsUsed[elemName].add(htmlLocation & ".children[" & $i & "]")
        sets.incl(jsFromOtherComponentsRequired, elemName)
        sets.incl(cssClassesRequired, components[elemName].cssClassesRequired)
        result &= components[elemName].initialHtml
        continue

      var properties = ""
      var j = 0
      while j < len(args):
        # TODO: Deduplicate this code
        var propName = ""
        var propValue = macros.newEmptyNode()
        if matches(args[j], asgn(ident(pn), pv)): propName = pn; propValue = pv
        elif matches(args[j], exprEqExpr(ident(pn), pv)): propName = pn; propValue = pv
        else: break

        if matches(propValue, blockStmt(_, stmtList(...body))):
          var jsCode = @["const componentRoot=event.target" & strutils.repeat(".parentElement", nestedLayersAfterComponentRoot)]
          for line in body:
            if matches(line, asgn(ident(varName), varValue)):
              checkVariableExists(data, varName, line[0])
              if data[varName].kind != State: macros.error("Cannot mutate `" & varName & "` because it is a " & $data[varName].kind & " variable")
              let (_, jsValue, _) = calculateDerivedValue(data, varValue)
              jsCode.add(data[varName].stateUpdateValueFuncName & "(componentRoot," & jsValue & ")")
            else:
              error("Did not match, got " & macros.treeRepr(line), line)
          properties &= " " & propName & "=\"" & jsCode.join(";") & "\""
        elif matches(propValue, strLit(propStrValue)):
          properties &= " " & propName & "=\"" & formatString(data, propStrValue, args[j][1], htmlLocation & "." & propName) & "\""
        else:
          discard
        j += 1
      var elemContentsAndEnd = ""
      case elemName
      of "input", "br":
        helpers.errorAssert(j == len(args), "Expect no element contents for `" & elemName & "` elements, but got element contents", args[j])
      else:
        elemContentsAndEnd =
          getInitialHtml(
            data,
            jsFromOtherComponentsRequired,
            cssClassesRequired,
            args[j..<len(args)],
            htmlLocation & ".children[" & $i & "]",
            nestedLayersAfterComponentRoot+1,
            otherComponentsUsed,
          ) & "</" & elemName & ">"
      result &= "<" & elemName & properties & ">" & elemContentsAndEnd
    else:
      macros.error("Did not match, got " & macros.treeRepr(elements[i]), elements[i])

proc defineVariable(data: var tables.Table[string, VariableInfo], varName: string, varNameNode: NimNode, varValue: NimNode) =
  helpers.errorAssert(not tables.hasKey(data, varName), "Redeclaration of variable called " & varName, varNameNode)

  if matches(varValue, call(ident("state"), value)):
    let valueData =
      if matches(value, intLit(v)): Data(kind: Int, intVal: v)
      elif matches(value, strLit(v)): Data(kind: String, strVal: v)
      else: macros.error("Does not match", value)
    data[varName] = VariableInfo(
      kind: State,
      stateUpdateValueFuncName: getNextFunctionName(),
      stateUpdateValueFuncContents: @["if (newValue == componentRoot.getAttribute(`state_" & varName & "`)) {return};componentRoot.setAttribute(`state_" & varName & "`," & "newValue)"],
      stateInitialValue: valueData,
    )
  elif matches(varValue, call(ident("derived"), value)):
    let (initialVal, updateValueJs, dependencies) = calculateDerivedValue(data, value)
    if sets.len(dependencies) == 0:
      macros.error("Unnecersarry call to `derived`", value)
    let funcName = getNextFunctionName()
    let updateDependencyCode = funcName & "(componentRoot)"
    var hasVariableDependencies = false
    for dep in sets.items(dependencies):
      hasVariableDependencies = hasVariableDependencies or data[dep].addToUpdateValueFunc(updateDependencyCode)
    if hasVariableDependencies == false:
      macros.error("Unnecersarry call to `derived`", value)
    data[varName] = VariableInfo(
      kind: Derived,
      derivedInitialValue: initialVal,
      recalculateValueFuncContents: @["componentRoot.setAttribute(`state_" & varName & "`," & updateValueJs & ")"],
      recalculateValueFuncName: funcName,
    )
  elif matches(varValue, call(ident("arg"))):
    # TODO: Add support for arguments
    data[varName] = VariableInfo()
  else:
    macros.error("Does not match", varValue)

macro component*(name: untyped, node: untyped) =
  helpers.errorAssert(name.kind == macros.nnkIdent, "Expected macro name as identifier", name)
  let componentName = macros.strVal(name)
  helpers.errorAssert(not tables.hasKey(components, componentName), "Component called `" & componentName & "` is already defined. Use a different name.", name)
  helpers.expectType(node, macros.nnkStmtList)

  # Parse the components data
  var data = tables.initTable[string, VariableInfo]()
  var i = 0
  var userOnloadBody = ""
  while i < macros.len(node):
    if matches(node[i], asgn(ident(propName), propValue)):
      if propName == "onload":
        if matches(propValue, tripleStrLit(onloadFunctionBody)):
          helpers.errorAssert(userOnloadBody == "", "The onload function can only be set once", node[i])
          helpers.errorAssert(onloadFunctionBody != "", "Setting the onload function to \"\" has no effect", node[i])
          userOnloadBody = onloadFunctionBody
        else:
          macros.error("Expected string literal for the body of the onload function, but got " & macros.treeRepr(node[i][1]))
      else:
        defineVariable(data, propName, node[i][0], propValue)
    else:
      break
    i += 1
  var initialState = ""
  for name, value in tables.pairs(data):
    let initialValueStr = optionalDataToOptionalString(value.initialValue)
    if options.isSome(initialValueStr):
      initialState &= " state_" & name & "=\"" & options.get(initialValueStr) & "\""

  # Parse the components markup
  var requiredClasses = sets.initHashSet[string]() # TODO: Add tailwind style classes for styling
  var jsFromOtherComponentsRequired = sets.initHashSet[string]()
  var otherComponentsUsed = tables.initTable[string, seq[string]]()
  let nodeContents = node[i..macros.len(node)-1]
  helpers.errorAssert(len(nodeContents) > 0, "Node does not have any contents", node)
  let initialHtml = "<div" & initialState & ">" & getInitialHtml(data, jsFromOtherComponentsRequired, requiredClasses, nodeContents, "componentRoot", 1, otherComponentsUsed) & "</div>"

  # Parse onload function
  var onloadBody = newSeq[string]()
  for compName, compUses in tables.pairs(otherComponentsUsed):
    if components[compName].onLoadFunctionName != "":
      for use in compUses:
        onloadBody.add(components[compName].onLoadFunctionName & "(" & use & ")")
  if userOnloadBody != "":
    onloadBody.add(userOnloadBody)
  var onloadName = ""
  var onloadFunc = ""
  if len(onloadBody) > 0:
    for name, value in tables.pairs(data):
      if value.kind == State:
        onloadBody.insert("const get_" & name & " = () => componentRoot.getAttribute(\"state_" & name & "\")")
        onloadBody.insert("const set_" & name & " = (newValue) => " & value.stateUpdateValueFuncName & "(componentRoot, newValue)")
    onloadName = getNextFunctionName()
    onloadFunc = "async function " & onloadName & "(componentRoot){" & onloadBody.join(";") & "}"

  # Parse the JS in order for the component to be interactive
  var componentJs = if onloadFunc != "": @[onloadFunc] else: newSeq[string]()
  for varInfo in tables.values(data):
    let stateUpdateFunction = stateUpdateFunction(varInfo)
    if options.isNone(stateUpdateFunction): continue
    let stateUpdateFunc = options.get(stateUpdateFunction)
    if len(stateUpdateFunc.contents) == 0: continue
    let stateUpdateFuncArgs = "componentRoot" & (if stateUpdateFunc.hasNewValueArg: ", newValue" else: "")
    let stateUpdateFuncBody = stateUpdateFunc.contents.join(";")
    componentJs.add("function " & stateUpdateFunc.name & "(" & stateUpdateFuncArgs & "){" & stateUpdateFuncBody & "}")
  
  # Return
  components[componentName] = Component(
    cssClassesRequired: requiredClasses,
    initialHtml: initialHtml,
    componentJs: componentJs.join(" "),
    jsFromOtherComponentsRequired: jsFromOtherComponentsRequired,
    onloadFunctionName: onloadName,
  )

macro page*(rootComponent: untyped): string =
  helpers.errorAssert(rootComponent.kind == macros.nnkIdent, "Expected root component of page as identifier", rootComponent)
  let componentName = macros.strVal(rootComponent)
  helpers.errorAssert(tables.hasKey(components, componentName), "Component called `" & componentName & "` is not defined", rootComponent)
  let rootComp = components[componentName]
  var js = rootComp.componentJs
  for comp in sets.items(rootComp.jsFromOtherComponentsRequired):
    js &= components[comp].componentJs
  let bodyProps = if rootComp.onloadFunctionName != "": " onload=\"" & rootComp.onloadFunctionName & "(event.target.children[0].children[1].children[0])\"" else: ""
  let res = """<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"></meta><title>TODO</title><meta name="viewport" content="width=device-width,initial-scale=1"></meta><script>""" & js & "</script></head><body" & bodyProps & ">" & rootComp.initialHtml & "</body></html>"
  return macros.newLit(res)
