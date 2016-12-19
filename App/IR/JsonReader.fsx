module App.IR

type Node(node_id, node_type) =
  member this.Node_ID : int = node_id
  member this.Node_Type : string = node_type

type Vector<'T>(node_id, node_type, vec) =
  inherit Node(node_id, node_type)
  member this.vec = vec

type IndexedVector<'T>(node_id, node_type, vec, declarations) =
  inherit Vector<'T>(node_id, node_type, vec)
  member this.declarations = declarations

type NameMap() =
  inherit Node(node_id, node_type)
  member kv = null

type Type(node_id, node_type) =
  inherit Node(node_id, node_type)
  abstract width_bits : int
  default this.width_bits = 0

type Type_Base(node_id, node_type) =
  inherit Type(node_id, node_type)

type Type_Unknown(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type StatOrDecl(node_id, node_type) =
  inherit Node(node_id, node_type)

type Declaration(node_id, node_type, name, declid) =
  inherit StatOrDecl(node_id, node_type)
  member this.name = name
  member this.declid = declid

type Type_Declaration(node_id, node_type, name, declid) =
  inherit Type(node_id, node_type)
  member this.name = name
  member this.declid = declid

type Expression(node_id, node_type, type_) =
  inherit Node(node_id, node_type)
  member this.type_ : Type = type_

type Operation(node_id, node_type, type_) =
  inherit Expression(node_id, node_type, type_)

type Path(node_id, node_type, name, absolute) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.absolute = absolute

type Annotation(node_id, node_type, name, expr) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.expr = expr

type Annotations(node_id, node_type, annotations) =
  inherit Node(node_id, node_type)
  member this.annotations = annotations

type Direction = None | In | Out | InOut

type Type_Type(node_id, node_type, type_) =
  inherit Type(node_id, node_type)
  member this.type_ = type_

type Type_Boolean(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_State(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Bits(node_id, node_type, size, isSigned) =
  inherit Type_Base(node_id, node_type)
  member this.size = size
  member this.isSigned = isSigned

type Type_Varbits(node_id, node_type, size) =
  inherit Type_Base(node_id, node_type)
  member this.size = size

type Parameter(node_id, node_type, name, declid, annotations, direction) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.direction = direction

type ParameterList(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters = parameters

type Type_Var(node_id, node_type, name, declid) =
  inherit Type_Declaration(node_id, node_type, name, declid)

type Type_InfInt(node_id, node_type, name, declid) =
  inherit Type(node_id, node_type)
  member this.name = name
  member this.declid = declid

type Type_Dontcare(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Void(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_MatchKind(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type TypeParameters(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters = parameters

type StructField(node_id, node_type, name, declid, annotations, type_) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.type_ = type_

type Type_StructLike(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.fields = fields

type Type_Struct(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Union(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Header(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Set(node_id, node_type, elementType) =
  inherit Type(node_id, node_type)
  member this.elementType = elementType

type Type_Tuple(node_id, node_type, components) =
  inherit Type(node_id, node_type)
  member this.components = components

type Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.typeParameters = typeParameters

type Type_Package(node_id, node_type, name, declid, annotations, typeParameters, constructorParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.constructorParams = constructorParams

type Type_Parser(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams = applyParams

type Type_Control(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams = applyParams

type Type_Name(node_id, node_type, path) =
  inherit Type(node_id, node_type)
  member this.path = path

type Type_Stack(node_id, node_type, elementType, size) =
  inherit Type(node_id, node_type)
  member this.elementType = elementType
  member this.size = size

type Type_Specialized(node_id, node_type, baseType, arguments) =
  inherit Type(node_id, node_type)
  member this.baseType = baseType
  member this.arguments = arguments

type Type_SpecializedCanonical(node_id, node_type, baseType, arguments, substituted) =
  inherit Type(node_id, node_type)
  member this.baseType = baseType
  member this.arguments = arguments
  member this.substituted = substituted

type Declaration_ID(node_id, node_type, name, declid) =
  inherit Declaration(node_id, node_type, name, declid)

type Type_String(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Enum(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members = members

type Type_Table(node_id, node_type, table) =
  inherit Type(node_id, node_type)
  member this.table = table

type Type_ActionEnum(node_id, node_type, actionList) =
  inherit Type(node_id, node_type)
  member this.actionList = actionList

type Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type(node_id, node_type)
  member this.typeParameters = typeParameters
  member this.returnType = returnType
  member this. parameters = parameters

type Type_Method(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

type ArgumentInfo(node_id, node_type, type_) =
  inherit Node(node_id, node_type)
  member this.type_ = type_

type Type_MethodCall(node_id, node_type, typeArguments, returnType, arguments) =
  inherit Type(node_id, node_type)
  member this.typeParameters = typeArguments
  member this.returnType = returnType
  member this. parameters = arguments

type Type_Action(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

type Method(node_id, node_type, name, declid, type_, isAbstract, annotations) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ = type_
  member this.isAbstract = isAbstract
  member this.annotations = annotations

type Type_Typedef(node_id, node_type, name, declid, annotations, type_) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.type_ = type_

type Type_Extern(node_id, node_type, name, declid, typeParameters, methods, attributes, annotations) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.typeParameters = typeParameters
  member this.methods = methods
  member this.attributes = attributes
  member this.annotations = annotations

type Operation_Unary(node_id, node_type, type_, expr) =
  inherit Operation(node_id, node_type, type_)
  member this.expr = expr

type Neg(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type Cmpl(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type LNot(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type Operation_Binary(node_id, node_type, type_, left, right) =
  inherit Operation(node_id, node_type, type_)
  member this.left = left
  member this.right = right

type Operation_Ternary(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation(node_id, node_type, type_)
  member this.e0 = e0
  member this.e1 = e1
  member this.e2 = e2

type Operation_Relation(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Mul(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Div(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Mod(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Add(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Sub(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Shl(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Shr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Equ(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Neq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Lss(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Leq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Grt(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Geq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type BAnd(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type BOr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type BXor(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type LAnd(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type LOr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Literal(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)

type Constant(node_id, node_type, type_, value, base_) = 
  inherit Literal(node_id, node_type, type_)
  member this.value = value
  member this.base_ = base_

type BoolLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value = value

type StringLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value = value

type PathExpression(node_id, node_type, type_, path) = 
  inherit Expression(node_id, node_type, type_)
  member this.path = path

type TypeNameExpression(node_id, node_type, type_, typeName) = 
  inherit Expression(node_id, node_type, type_)
  member this.typeName = typeName

type Slice(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation_Ternary(node_id, node_type, type_, e0, e1, e2)

type Member(node_id, node_type, type_, expr, member_) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  member this.member_ = member_

type Concat(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type ArrayIndex(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Range(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Mask(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

type Mux(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation_Ternary(node_id, node_type, type_, e0, e1, e2)

type DefaultExpression(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)

type This(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)

type Cast(node_id, node_type, destType, expr) =
  inherit Operation_Unary(node_id, node_type, destType, expr)
  member this.destType = destType

type SelectCase(node_id, node_type, keyset, state) =
  inherit Node(node_id, node_type)
  member this.keyset = keyset
  member this.state = state

type SelectExpression(node_id, node_type, type_, select, selectCases) = 
  inherit Expression(node_id, node_type, type_)
  member this.select = select
  member this.selectCases = selectCases

type MethodCallExpression(node_id, node_type, type_, method_, typeArguments, arguments) = 
  inherit Expression(node_id, node_type, type_)
  member this.method_ = method_
  member this.typeArguments = typeArguments
  member this.arguments = arguments

type ConstructorCallExpression(node_id, node_type, type_, arguments) = 
  inherit Expression(node_id, node_type, type_)
  member this.constructedType = this.type_
  member this.arguments = arguments

type ListExpression(node_id, node_type, type_, components) = 
  inherit Expression(node_id, node_type, type_)
  member this.components = components

type ParserState(node_id, node_type, name, declid, annotations, components, selectExpression) = 
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.components = components
  member this.selectExpression = selectExpression

type P4Parser(node_id, node_type, name, declid, type_, constructorParams, parserLocals, states) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.type_ = type_
  member this.constructorParams = constructorParams
  member this.parserLocals = parserLocals
  member this.states = states

type P4Control(node_id, node_type, name, declid, type_, constructorParams, controlLocals, body) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.type_ = type_
  member this.constructorParams = constructorParams
  member this.controlLocals = controlLocals
  member this.body = body

type P4Action(node_id, node_type, name, declid, annotations, parameters, body) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.parameters = parameters
  member this.body = body

type Type_Error(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members = members

type Declaration_MatchKind(node_id, node_type, members) =
  inherit Node(node_id, node_type)
  member this.members = members

type PropertyValue(node_id, node_type) =
  inherit Node(node_id, node_type)

type ExpressionValue(node_id, node_type, expression) =
  inherit PropertyValue(node_id, node_type)
  member this.expression = expression

type ExpressionListValue(node_id, node_type, expressions) =
  inherit PropertyValue(node_id, node_type)
  member this.expressions = expressions

type ActionListElement(node_id, node_type, annotations, expression) =
  inherit Node(node_id, node_type)
  member this.annotations = annotations
  member this.expression = expression

type ActionList(node_id, node_type, actionList) =
  inherit PropertyValue(node_id, node_type)
  member this.actionList = actionList

type KeyElement(node_id, node_type, annotations, expression, matchType) =
  inherit Node(node_id, node_type)
  member this.annotations = annotations
  member this.expression = expression
  member this.matchType = matchType

type Key(node_id, node_type, keyElements) =
  inherit PropertyValue(node_id, node_type)
  member this.keyElements = keyElements

type Property(node_id, node_type, name, declid, annotations, value) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.value = value

type TableProperties(node_id, node_type, properties) =
  inherit Node(node_id, node_type)
  member this.properties = properties

type P4Table(node_id, node_type, name, declid, annotations, parameters, properties) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.parameters = parameters
  member this.properties = properties

type Declaration_Variable(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.type_ = type_
  member this.initializer = initializer

type Declaration_Constant(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.type_ = type_
  member this.initializer = initializer

type Declaration_Instance(node_id, node_type, name, declid, annotations, type_, arguments, properties, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations = annotations
  member this.type_ = type_
  member this.arguments = arguments
  member this.properties = properties
  member this.initializer = initializer

type P4Program(node_id, node_type, declarations) =
  inherit Node(node_id, node_type)
  member this.declarations = declarations

type Statement(node_id, node_type) =
  inherit StatOrDecl(node_id, node_type)

type ExitStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

type ReturnStatement(node_id, node_type, expression) =
  inherit Statement(node_id, node_type)
  member this.expression = expression

type EmptyStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

type AssignmentStatement(node_id, node_type, left, right) =
  inherit Statement(node_id, node_type)
  member this.left = left
  member this.right = right

type IfStatement(node_id, node_type, condition, ifTrue, ifFalse) =
  inherit Statement(node_id, node_type)
  member this.condition = condition
  member this.ifTrue = ifTrue
  member this.ifFalse = ifFalse

type BlockStatement(node_id, node_type, annotations, components) =
  inherit Statement(node_id, node_type)
  member this.annotations = annotations
  member this.components = components

type MethodCallStatement(node_id, node_type, methodCall) =
  inherit Statement(node_id, node_type)
  member this.methodCall = methodCall

type SwitchCase(node_id, node_type, label, statement) =
  inherit Node(node_id, node_type)
  member this.label = label
  member this.statement = statement

type SwitchStatement(node_id, node_type, expressions, cases) =
  inherit Statement(node_id, node_type)
  member this.expressions = expressions
  member this.cases = cases

type Function(node_id, node_type, name, declid, type_, body) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ = type_
  member this.body = body

type Block(node_id, node_type, node, constantVaue) =
  inherit Node(node_id, node_type)
  member this.node = node
  member this.constantValue = constantValue

type TableBlock(node_id, node_type, node, constantValue, container) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.container = container

type InstantiatedBlock(node_id, node_type, node, constantValue, instanceType) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.instanceType = instanceType

type ParserBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container = container

type ControlBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container = container

type PackageBlock(node_id, node_type, node, constantValue, instanceType, type_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.type_ = type_

type ExternBlock(node_id, node_type, node, constantValue, instanceType, type_, constructor_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.type_ = type_
  member this.constructor_ = constructor_

type TopLevelBlock(node_id, node_type, node, constantValue) =
  inherit Block(node_id, node_type, node, constantValue)

type CounterType = None | Packets | Bytes | Both with
  static member parse s =
    match s with
    | "NONE" -> None
    | "PACKETS" -> Packets
    | "BYTES" -> Bytes
    | "BOTH" -> Both
    | _ -> failwith "ERROR"
  static member toString c =
    match c with
    | None -> "NONE"
    | Packets -> "PACKETS"
    | Bytes -> "BYTES"
    | Both -> "BOTH"

type Type_Block(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Counter(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Expression(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_FieldListCalculation(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Meter(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Register(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_AnyTable(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_) =
  inherit Node(node_id, node_type)
  member this.type_name = type_name
  member this.name = name
  member this.annotations = annotations
  member this.type_ = type_

type Header(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

type HeaderStack(node_id, node_type, type_name, name, annotations, type_, size) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)
  member this.size = size

type Metadata(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

type HeaderRef(node_id, node_type, type_name, name, annotations, type_) =
  inherit Header(node_id, node_type, type_name, name, annotations, type_)

type ConcreteHeaderRef(node_id, node_type, type_name, name, annotations, type_, ref) =
  inherit HeaderRef(node_id, node_type, type_name, name, annotations, type_)
  member this.ref = ref

type HeaderStackItemRef(node_id, node_type, type_name, name, annotations, type_, base_, index_) =
  inherit HeaderRef(node_id, node_type, type_name, name, annotations, type_)
  member this.base_ = base_
  member this.index_ = index_

type NamedRef(node_id, node_type, type_, name) =
  inherit Expression(node_id, node_type, type_)
  member this.name = name

type If(node_id, node_type, type_, pred, ifTrue, ifFalse) =
  inherit Expression(node_id, node_type, type_)
  member this.pred = pred
  member this.ifTrue = ifTrue
  member this.ifFalse = ifFalse

type NamedCond(node_id, node_type, type_, pred, ifTrue, ifFalse, name) =
  inherit If(node_id, node_type, type_, pred, ifTrue, ifFalse)
  member this.name = name

type Apply(node_id, node_type, type_, name, actions) =
  inherit Expression(node_id, node_type, type_)
  member this.name = name
  member this.actions = actions

type Primitive(node_id, node_type, type_, name, operands) =
  inherit Operation(node_id, node_type, type_)
  member this.name = name
  member this.operands = operands

type FieldList(node_id, node_type, name, payload, annotations, fields) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.payload = payload
  member this.annotations = annotations
  member this.fields = fields

type FieldListCalculation(node_id, node_type, name, input, algorithm, annotations) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.input = input
  member this.algorithm = algorithm
  member this.annotations = annotations

type CalculatedField(node_id, node_type, field, name, cond) =
  inherit Node(node_id, node_type)
  member this.field = field
  member this.name = name
  member this.cond = cond

type CaseEntry(node_id, node_type, values, action) =
  inherit Node(node_id, node_type)
  member this.values = values
  member this.action = action

type V1Parser(node_id, node_type, name, stmts, select, cases, default_return, parse_error, annotations) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.stmts = stmts
  member this.select = select
  member this.cases = cases
  member this.default_return = default_return
  member this.parse_error = parse_error
  member this.annotations = annotations

type ParserException(node_id, node_type) =
  inherit Node(node_id, node_type)

type Attached(node_id, node_type, name, annotations) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.annotations = annotations

type Stateful(node_id, node_type, name, annotations, table) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.table = table

type CounterOrMeter(node_id, node_type, name, annotations, table, type_) =
  inherit Stateful(node_id, node_type, name, annotations, table)
  member this.type_ = type_

type Counter(node_id, node_type, name, annotations, table, type_) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, type_)

type  Meter(node_id, node_type, name, annotations, table, result, pre_color, implementation) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, type_)
  member this.result = result
  member this.pre_color = pre_color
  member this.implementation = implementation

type Register(node_id, node_type, name, annotations, table, layout, width, signed_) =
  inherit Stateful(node_id, node_type, name, annotations, table)
  member this.layout = layout
  member this.width = width
  member this.signed_ = signed_

type PrimitiveAction(node_id, node_type) =
  inherit Node(node_id, node_type)

type NameList(node_id, node_type, names) =
  inherit Node(node_id, node_type)
  member this.names = names

type ActionArg(node_id, node_type, type_, action_name, name) =
  inherit Expression(node_id, node_type, type_)
  member this.action_name = action_name
  member this.name = name

type ActionFunction(node_id, node_type, name, action, args, annotations) =
  inherit Node(node_id, node_type)
  member this.name = name
  member this.action = action
  member this.args = args
  member this.annotations = annotations

type ActionProfile(node_id, node_type, name, annotations, selector, actions, size) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.selector = selector
  member this.actions = actions
  member this.size = size

type ActionSelector(node_id, node_type, name, annotations, key, mode, type_) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.key = key
  member this.mode = mode
  member this.type_ = type_

type V1Table(node_id, node_type, ...) = //FIXME ignored loads of fields
  inherit Node(node_id, node_type)

type V1Control(node_id, node_type, ...) = //FIXME ignored loads of fields
  inherit Node(node_id, node_type)

type Attribute(node_id, node_type. name, declid, type_, locals, optional) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ = type_
  member this.locals = locals
  member this.optional = optional

type V1Program(node_id, node_type, ...) = //FIXME ignored loads of fields
  inherit Node(node_id, node_type)

type V1HeaderType(node_id, node_type, ...) = //FIXME ignored loads of fields
  inherit Node(node_id, node_type)

type IntMod(node_id, node_type, type_, expr, width) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  member this.width = width

