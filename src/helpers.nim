from macros import `[]`
from std/sequtils import map
from std/genasts import genAst
from std/options import nil
from std/sets import contains

macro debugMacrosUntyped*(body: untyped) =
  echo macros.astGenRepr(body)

macro debugMacros*(body: typed) =
  echo macros.strVal(macros.toStrLit(body))

func repr*[T] (option: options.Option[T]): string =
  if options.isSome(option):
    return "options.some(" & repr(options.get(option)) & ")"
  return "options.none(" & repr(type(option)) & ")"

func listToStr[T](list: varargs[T]): string =
  case len(list)
  of 0:
    return ""
  of 1:
    return $list[0]
  else:
    for i in 0..len(list)-2:
      result &= $list[i] & ", "
    result &= "or " & $list[len(list)-1]

template errorAssert*(condition: bool, msg: string, location: NimNode) =
  # This is a template so that `msg` and `location` are not evaluated unless condition is false
  if not condition:
    macros.error(msg, location)

func expectType*(node: NimNode, expectedTypes: varargs[macros.NimNodeKind]) =
  errorAssert(
    macros.kind(node) in sets.toHashSet(expectedTypes),
    "Expected node of type " & listToStr(expectedTypes) & ", but got node of type " & $macros.kind(node),
    node,
  )

template update(ast: var untyped, args: varargs[untyped]): untyped =
  ast = genAst(prevAst = ast, args)

type capturedVariable = object
  varType: NimNode
  varName: string
  initialValue: NimNode

func matchesFunc(got: NimNode, expected: NimNode, check: var NimNode, capturedVariables: var seq[capturedVariable]) =
  expectType(expected, macros.nnkCall, macros.nnkIdent)

  # Check the node type
  if macros.kind(expected) == macros.nnkIdent:
    if macros.strVal(expected) != "_":
      capturedVariables.add(capturedVariable(
        varType: macros.newIdentNode("NimNode"),
        varName: macros.strVal(expected),
        initialValue: genAst(got, got),
      ))
    return

  case macros.strVal(expected[0])
    of "stmtList":   check.update(got, prevAst and macros.kind(got) == macros.nnkStmtList)
    of "par":        check.update(got, prevAst and macros.kind(got) == macros.nnkPar)
    of "returnStmt": check.update(got, prevAst and macros.kind(got) == macros.nnkReturnStmt)
    of "call":       check.update(got, prevAst and macros.kind(got) == macros.nnkCall)
    of "asgn":       check.update(got, prevAst and macros.kind(got) == macros.nnkAsgn)
    of "exprEqExpr": check.update(got, prevAst and macros.kind(got) == macros.nnkExprEqExpr)
    of "blockStmt":  check.update(got, prevAst and macros.kind(got) == macros.nnkBlockStmt)
    of "infix":      check.update(got, prevAst and macros.kind(got) == macros.nnkInfix)

    # TODO: Deduplicate the code in these 3 of clauses
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
      return
    of "ident":
      errorAssert(macros.len(expected) == 2, "Expected 1 argument (value of identifier)", expected)
      let expectedName = expected[1]
      expectType(expectedName, macros.nnkIdent, macros.nnkStrLit)
      check.update(got, prevAst and got.kind == macros.nnkIdent)
      if macros.kind(expectedName) == macros.nnkStrLit:
        check.update(got, expectedValue = macros.strVal(expectedName)):
          prevAst and macros.strVal(got) == expectedValue
      elif macros.strVal(expectedName) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("string"),
          varName: macros.strVal(expectedName),
          initialValue: genAst(got, macros.strVal(got)),
        ))
      return
    of "strLit":
      errorAssert(macros.len(expected) == 2, "Expected 1 argument (value of string)", expected)
      let expectedName = expected[1]
      expectType(expectedName, macros.nnkIdent, macros.nnkStrLit)
      check.update(got, prevAst and got.kind == macros.nnkStrLit)
      if macros.kind(expectedName) == macros.nnkStrLit:
        check.update(got, expectedValue = macros.strVal(expectedName)):
          prevAst and macros.strVal(got) == expectedValue
      elif macros.strVal(expectedName) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("string"),
          varName: macros.strVal(expectedName),
          initialValue: genAst(got, macros.strVal(got)),
        ))
      return
    of "tripleStrLit":
      errorAssert(macros.len(expected) == 2, "Expected 1 argument (value of string)", expected)
      let expectedName = expected[1]
      expectType(expectedName, macros.nnkIdent, macros.nnkStrLit)
      check.update(got, prevAst and got.kind == macros.nnkTripleStrLit)
      if macros.kind(expectedName) == macros.nnkStrLit:
        check.update(got, expectedValue = macros.strVal(expectedName)):
          prevAst and macros.strVal(got) == expectedValue
      elif macros.strVal(expectedName) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newIdentNode("string"),
          varName: macros.strVal(expectedName),
          initialValue: genAst(got, macros.strVal(got)),
        ))
      return

    else:
      if macros.strVal(expected[0]) != "_":
        capturedVariables.add(capturedVariable(
          varType: macros.newTree(macros.nnkDotExpr, macros.newIdentNode("macros"), macros.newIdentNode("NimNodeKind")),
          varName: macros.strVal(expected[0]),
          initialValue: genAst(got, macros.kind(got)),
        ))

  let expectedElems = expected[1..macros.len(expected)-1]

  # Get the index of the spread capture (if there is one)
  var spreadCaptureIndex = len(expectedElems)
  for i, elem in expectedElems.pairs:
    if macros.kind(elem) == macros.nnkPrefix and macros.strVal(elem[0]) == "...":
      errorAssert(spreadCaptureIndex == len(expectedElems), "Multiple spread captures in the same node are not supported", elem)
      spreadCaptureIndex = i

  # Compare the length of got and expected
  if spreadCaptureIndex == len(expectedElems):
    check.update(got, expectedLen = len(expectedElems)):
      prevAst and macros.len(got) == expectedLen
  else:
    check.update(got, expectedLen = len(expectedElems) - 1):
      prevAst and macros.len(got) >= expectedLen

  # Check the node items before the spread capture
  for i in 0..<spreadCaptureIndex:
    matchesFunc(genAst(got, i, got[i]), expectedElems[i], check, capturedVariables)

  # Check the spread capture
  if spreadCaptureIndex == len(expectedElems):
    return
  let name = macros.strVal(expectedElems[spreadCaptureIndex][1])
  if name != "_":
    capturedVariables.add(capturedVariable(
      varType: macros.newTree(macros.nnkBracketExpr, macros.newIdentNode("seq"), macros.newIdentNode("NimNode")),
      varName: name,
      initialValue: genAst(got, spreadCaptureIndex, expectedElemsLeft=len(expectedElems)-spreadCaptureIndex, got[spreadCaptureIndex..macros.len(got)-expectedElemsLeft])
    ))

  # Check the node items after the spread capture
  var elementsLeft = len(expectedElems)-spreadCaptureIndex-1
  while elementsLeft >= 1:
    matchesFunc(genAst(got, elementsLeft, got[macros.len(got)-elementsLeft]), expectedElems[len(expectedElems) - elementsLeft], check, capturedVariables)
    elementsLeft -= 1

func combine*[T](dataIn: seq[T], combineFunc: proc(a: T, b: T): T {.noSideEffect.}): options.Option[T] =
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
