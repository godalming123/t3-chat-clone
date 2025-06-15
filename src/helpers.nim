from macros import `[]`
from std/sequtils import map
from std/sugar import collect
from std/genasts import genAst
from std/options import nil

macro debugMacrosUntyped*(body: untyped) =
  echo macros.astGenRepr(body)

macro debugMacros*(body: typed) =
  echo macros.strVal(macros.toStrLit(body))

func listToStr(list: seq[string]): string =
  case len(list)
  of 0:
    return ""
  of 1:
    return list[0]
  else:
    for i in 0..len(list)-2:
      result &= list[i] & ", "
    result &= "or " & list[len(list)-1]

template errorAssert(condition: bool, msg: string, location: NimNode) =
  # This is a template so that `msg` and `location` are not evaluated unless condition is false
  if not condition:
    macros.error(msg, location)

func expectType(node: NimNode, firstExpectedType: macros.NimNodeKind, otherExpectedTypes: varargs[macros.NimNodeKind]) =
  if macros.kind(node) == firstExpectedType: return
  for i in 0..len(otherExpectedTypes)-1:
    if macros.kind(node) == otherExpectedTypes[i]: return
  let expectedTypeStrings = collect:
    for elem in sequtils.concat(@[firstExpectedType], sequtils.toSeq(otherExpectedTypes)):
      $elem
  macros.error("Expected node of type " & listToStr(expectedTypeStrings) & ", but got node of type " & $macros.kind(node), node)

template update(ast: var untyped, args: varargs[untyped]): untyped =
  ast = genAst(prevAst = ast, args)

type capturedVariable = object
  varType: NimNode
  varName: string
  initialValue: NimNode

func checkAstHead(got: NimNode, expected: NimNode, check: var NimNode, capturedVariables: var seq[capturedVariable]): (int, int) =
  case macros.strVal(expected[0])
    of "call":
      let varIdent = expected[1]
      expectType(varIdent, macros.nnkIdent, macros.nnkStrLit)
      check.update(got, expectedLen = macros.len(expected)-1):
        prevAst and macros.kind(got) == macros.nnkCall and macros.len(got) == expectedLen
      let name = macros.strVal(varIdent)
      if macros.kind(varIdent) == macros.nnkStrLit:
        check.update(got, name, prevAst and (macros.strVal(got[0]) == name))
      elif name != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("string"),
          varName: name,
          initialValue: genAst(got, varIdent, varIdent = macros.strVal(got[0])),
        ))
      return (1, 2)

    of "intLit":
      errorAssert(macros.len(expected) == 2, "Expected 1 argument (value of int literal)", expected)
      let expectedInt = expected[1]
      expectType(expectedInt, macros.nnkIdent, macros.nnkIntLit)
      check.update(got, prevAst and got.kind == macros.nnkIntLit)
      if macros.kind(expectedInt) == macros.nnkIntLit:
        check.update(got, expectedValue = macros.intVal(expectedInt)):
          prevAst and macros.intVal(got) == expectedValue
      elif macros.strVal(expectedInt) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("int"),
          varName: macros.strVal(expectedInt),
          initialValue: genAst(got, macros.intVal(got)),
        ))
      return (0, 2)

    of "stmtList":
      check.update(got, expectedLen = macros.len(expected)-1):
        prevAst and macros.len(got) == expectedLen and got.kind == macros.nnkStmtList
      return (0, 1)

    of "asgn":
      errorAssert(macros.len(expected) == 3, "Expected 2 arguments (name of variable being assigned to and value being assigned)", expected)
      expectType(expected[1], macros.nnkIdent, macros.nnkIntLit)
      check.update(got, prevAst and got.kind == macros.nnkAsgn)
      let name = macros.strVal(expected[1])
      if macros.kind(expected[1]) == macros.nnkStrLit:
        check.update(got, name, prevAst and macros.strVal(got[0]) == name)
      elif name != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("string"),
          varName: name,
          initialValue: genAst(got, macros.strVal(got[0])),
        ))
      return (1, 2)

    else:
      if macros.strVal(expected[0]) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newTree(macros.nnkDotExpr, macros.newIdentNode("macros"), macros.newIdentNode("NimNodeKind")),
          varName: macros.strVal(expected[0]),
          initialValue: genAst(got, macros.kind(got)),
        ))
      check.update(got, expectedLen = macros.len(expected)):
        prevAst and macros.len(got) == expectedLen
      return (0, 1)

func matchesFunc(got: NimNode, expected: NimNode, check: var NimNode, capturedVariables: var seq[capturedVariable]) =
  expectType(expected, macros.nnkCall, macros.nnkIdent)

  # Check that the node type is the same
  if macros.kind(expected) == macros.nnkIdent:
    if macros.strVal(expected) != "_":
      capturedVariables.add(capturedVariable(
        varType: macros.newIdentNode("NimNode"),
        varName: macros.strVal(expected),
        initialValue: genAst(got, got),
      ))
    return
  var (gotIndex, expectedIndex) = checkAstHead(got, expected, check, capturedVariables)

  # Check that the node items are the same
  while expectedIndex < macros.len(expected):
    if macros.kind(expected[expectedIndex]) == macros.nnkPrefix and macros.strVal(expected[expectedIndex][0]) == "...":
      let name = macros.strVal(expected[expectedIndex][1])
      if name != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newTree(macros.nnkBracketExpr, macros.newIdentNode("seq"), macros.newIdentNode("NimNode")),
          varName: name,
          initialValue: genAst(got, gotIndex, got[gotIndex..macros.len(got)-1]),
        ))
      errorAssert(expectedIndex == macros.len(expected)-1, "Elements after spread capture are not yet supported", expected[expectedIndex+1])
      return

    matchesFunc(genAst(got, gotIndex, got[gotIndex]), expected[expectedIndex], check, capturedVariables)
    gotIndex += 1
    expectedIndex += 1

func combine[T](dataIn: seq[T], combineFunc: proc(a: T, b: T): T {.noSideEffect.}): options.Option[T] =
  if len(dataIn) == 0:
    return options.none(T)
  var funcOut = dataIn[0]
  for i in 1..len(dataIn)-1:
    funcOut = combineFunc(funcOut, dataIn[i])
  return options.some(funcOut)

func combineStatements(statements: seq[NimNode]): options.Option[NimNode] =
  return statements.combine(func(a, b: NimNode): NimNode = genAst(a, b):
    a
    b
  )

macro matches*(got: untyped, expected: untyped): untyped =
  var check = macros.newLit(true)
  var capturedVariables: seq[capturedVariable] = @[]
  matchesFunc(got, expected, check, capturedVariables)
  let varDecleration = capturedVariables.map(proc(v: capturedVariable): NimNode =
    macros.newTree(macros.nnkVarSection, macros.newTree(macros.nnkIdentDefs, macros.newIdentNode(v.varName), v.varType, macros.newEmptyNode()))
  ).combineStatements()
  let varInitialisation = capturedVariables.map(proc(v: capturedVariable): NimNode =
    macros.newTree(macros.nnkAsgn, macros.newIdentNode(v.varName), v.initialValue)
  ).combineStatements()
  if options.isSome(varDecleration) and options.isSome(varInitialisation):
    return genAst(varDecl = options.get(varDecleration), varInitialisation = options.get(varInitialisation), check):
      (varDecl;
      if check:
        varInitialisation
        true
      else:
        false)
  else:
    assert((not options.isSome(varDecleration)) and (not options.isSome(varInitialisation)))
    return check
