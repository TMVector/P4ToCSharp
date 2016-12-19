module App.IR

// Original IR uses a struct with original name and source info, but only name is serialised
type ID = string

// Marker interfaces
type INode = interface end
type IDeclaration = inherit INode
type IContainer = inherit IDeclaration
type ICompileTimeValue = inherit INode

// Use this as ordered map for now
type OrderedMap<'K, 'V> = ('K * 'V) array

type Node(node_id, node_type) =
  interface INode
  member this.Node_ID : int = node_id
  member this.Node_Type : string = node_type

type Vector<'T>(node_id, node_type, vec) =
  inherit Node(node_id, node_type)
  member this.vec : 'T array = vec // value is json list

type IndexedVector<'T>(node_id, node_type, vec, declarations) =
  inherit Vector<'T>(node_id, node_type, vec)
  member this.declarations : System.Collections.Generic.IDictionary<string, IDeclaration> = declarations // value is json list

type NameMap<'T>(node_id, node_type, symbols) =
  inherit Node(node_id, node_type)
  member this.symbols : System.Collections.Generic.IDictionary<string, 'T> = symbols // value is json dictionary

type Type(node_id, node_type) =
  inherit Node(node_id, node_type)

type Type_Base(node_id, node_type) =
  inherit Type(node_id, node_type)

type Type_Unknown(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type StatOrDecl(node_id, node_type) =
  inherit Node(node_id, node_type)

type Declaration(node_id, node_type, name, declid) =
  inherit StatOrDecl(node_id, node_type)
  interface IDeclaration
  member this.name : ID = name
  member this.declid : int = declid

type Type_Declaration(node_id, node_type, name, declid) =
  inherit Type(node_id, node_type)
  interface IDeclaration
  member this.name : ID = name
  member this.declid : int = declid

type Expression(node_id, node_type, type_) =
  inherit Node(node_id, node_type)
  member this.type_ : Type = type_

type Operation(node_id, node_type, type_) =
  inherit Expression(node_id, node_type, type_)

type Path(node_id, node_type, name, absolute) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.absolute : bool = absolute

type Annotation(node_id, node_type, name, expr) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.expr : Vector<Expression> = expr

type Annotations(node_id, node_type, annotations) =
  inherit Node(node_id, node_type)
  member this.annotations : Vector<Annotation> = annotations

type Direction = None | In | Out | InOut with
  static member toString d =
    match d with
    | None -> ""
    | In -> "in"
    | Out -> "out"
    | InOut -> "inout"
  static member parse s =
    match s with
    | "" -> None
    | "in" -> In
    | "out" -> Out
    | "inout" -> InOut

type Type_Type(node_id, node_type, type_) =
  inherit Type(node_id, node_type)
  member this.type_ : Type = type_ // type

type Type_Boolean(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_State(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Bits(node_id, node_type, size, isSigned) =
  inherit Type_Base(node_id, node_type)
  member this.size : int = size
  member this.isSigned : bool = isSigned

type Type_Varbits(node_id, node_type, size) =
  inherit Type_Base(node_id, node_type)
  member this.size : int = size

type Parameter(node_id, node_type, name, declid, annotations, direction, type_) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.direction : Direction = direction
  member this.type_ : Type = type_ // type

type ParameterList(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters : IndexedVector<Parameter> = parameters

type Type_Var(node_id, node_type, name, declid) =
  inherit Type_Declaration(node_id, node_type, name, declid)

type Type_InfInt(node_id, node_type, declid) =
  inherit Type(node_id, node_type)
  member this.declid : int = declid

type Type_Dontcare(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Void(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_MatchKind(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type TypeParameters(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters : IndexedVector<Type_Var> = parameters

type StructField(node_id, node_type, name, declid, annotations, type_) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.type_ : Type = type_ // type

type Type_StructLike(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.fields : IndexedVector<StructField> = fields

type Type_Struct(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Union(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Header(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

type Type_Set(node_id, node_type, elementType) =
  inherit Type(node_id, node_type)
  member this.elementType : Type = elementType

type Type_Tuple(node_id, node_type, components) =
  inherit Type(node_id, node_type)
  member this.components : Vector<Type> = components

type Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.typeParameters : TypeParameters = typeParameters

type Type_Package(node_id, node_type, name, declid, annotations, typeParameters, constructorParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  interface IContainer
  member this.constructorParams : ParameterList = constructorParams

type Type_Parser(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams : ParameterList = applyParams

type Type_Control(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams : ParameterList = applyParams

type Type_Name(node_id, node_type, path) =
  inherit Type(node_id, node_type)
  member this.path : Path = path

type Type_Stack(node_id, node_type, elementType, size) =
  inherit Type(node_id, node_type)
  member this.elementType : Type = elementType
  member this.size : Expression = size

type Type_Specialized(node_id, node_type, baseType, arguments) =
  inherit Type(node_id, node_type)
  member this.baseType : Type_Name = baseType
  member this.arguments : Vector<Type> = arguments

type Type_SpecializedCanonical(node_id, node_type, baseType, arguments, substituted) =
  inherit Type(node_id, node_type)
  member this.baseType : Type = baseType
  member this.arguments : Vector<Type> = arguments
  member this.substituted : Type = substituted

type Declaration_ID(node_id, node_type, name, declid) =
  inherit Declaration(node_id, node_type, name, declid)
  interface ICompileTimeValue

type Type_String(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type Type_Enum(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members : IndexedVector<Declaration_ID> = members

type Type_Table(node_id, node_type, table) =
  inherit Type(node_id, node_type)
  member this.table : P4Table = table

type Type_ActionEnum(node_id, node_type, actionList) =
  inherit Type(node_id, node_type)
  member this.actionList : ActionList = actionList

type Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type(node_id, node_type)
  member this.typeParameters : TypeParameters = typeParameters
  member this.returnType : Type = returnType // if != nullptr
  member this.parameters : ParameterList = parameters

type Type_Method(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

type ArgumentInfo(node_id, node_type, leftValue, compileTimeConstant, type_) =
  inherit Node(node_id, node_type)
  member this.leftValue : bool = leftValue
  member this.compileTimeConstant : bool = compileTimeConstant
  member this.type_ : Type = type_ // type

type Type_MethodCall(node_id, node_type, typeArguments, returnType, arguments) =
  inherit Type(node_id, node_type)
  member this.typeParameters : Vector<Type> = typeArguments
  member this.returnType : Type_Var = returnType
  member this.arguments : Vector<ArgumentInfo> = arguments

type Type_Action(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

type Method(node_id, node_type, name, declid, type_, isAbstract, annotations) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ : Type_Method = type_ // type
  member this.isAbstract : bool = isAbstract
  member this.annotations : Annotations = annotations

type Type_Typedef(node_id, node_type, name, declid, annotations, type_) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.type_ : Type = type_ // type

type Type_Extern(node_id, node_type, name, declid, typeParameters, methods, attributes, annotations) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.typeParameters : TypeParameters = typeParameters
  member this.methods : Vector<Method> = methods
  member this.attributes : NameMap<Attribute> = attributes // ordered_map
  member this.annotations : Annotations = annotations

type Operation_Unary(node_id, node_type, type_, expr) =
  inherit Operation(node_id, node_type, type_)
  member this.expr : Expression = expr

type Neg(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type Cmpl(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type LNot(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

type Operation_Binary(node_id, node_type, type_, left, right) =
  inherit Operation(node_id, node_type, type_)
  member this.left : Expression = left
  member this.right : Expression = right

type Operation_Ternary(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation(node_id, node_type, type_)
  member this.e0 : Expression = e0
  member this.e1 : Expression = e1
  member this.e2 : Expression = e2

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
  interface ICompileTimeValue

type Constant(node_id, node_type, type_, value, base_) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : bigint = value
  member this.base_ : uint32 = base_ // base

type BoolLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : bool = value

type StringLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : string = value

type PathExpression(node_id, node_type, type_, path) = 
  inherit Expression(node_id, node_type, type_)
  member this.path : Path = path

type TypeNameExpression(node_id, node_type, type_, typeName) = 
  inherit Expression(node_id, node_type, type_)
  member this.typeName : Type_Name = typeName

type Slice(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation_Ternary(node_id, node_type, type_, e0, e1, e2)

type Member(node_id, node_type, type_, expr, member_) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  member this.member_ : ID = member_ // member

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
  member this.destType : Type = destType

type SelectCase(node_id, node_type, keyset, state) =
  inherit Node(node_id, node_type)
  member this.keyset : Expression = keyset
  member this.state : PathExpression = state

type SelectExpression(node_id, node_type, type_, select, selectCases) = 
  inherit Expression(node_id, node_type, type_)
  member this.select : ListExpression = select
  member this.selectCases : Vector<SelectCase> = selectCases

type MethodCallExpression(node_id, node_type, type_, method_, typeArguments, arguments) = 
  inherit Expression(node_id, node_type, type_)
  member this.method_ : Expression = method_ // method
  member this.typeArguments : Vector<Type> = typeArguments
  member this.arguments : Vector<Expression> = arguments

type ConstructorCallExpression(node_id, node_type, type_, arguments) = 
  inherit Expression(node_id, node_type, type_)
  member this.constructedType : Type = this.type_
  member this.arguments : Vector<Expression> = arguments

type ListExpression(node_id, node_type, type_, components) = 
  inherit Expression(node_id, node_type, type_)
  member this.components : Vector<Expression> = components

type ParserState(node_id, node_type, name, declid, annotations, components, selectExpression) = 
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.components : IndexedVector<StatOrDecl> = components
  member this.selectExpression : Expression = selectExpression // if != nullptr

type P4Parser(node_id, node_type, name, declid, type_, constructorParams, parserLocals, states) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  interface IContainer
  member this.type_ : Type_Parser = type_ // type
  member this.constructorParams : ParameterList = constructorParams
  member this.parserLocals : IndexedVector<Declaration> = parserLocals
  member this.states : IndexedVector<ParserState> = states

type P4Control(node_id, node_type, name, declid, type_, constructorParams, controlLocals, body) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  interface IContainer
  member this.type_ : Type_Control = type_ // type
  member this.constructorParams : ParameterList = constructorParams
  member this.controlLocals : IndexedVector<Declaration> = controlLocals
  member this.body : BlockStatement = body

type P4Action(node_id, node_type, name, declid, annotations, parameters, body) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.parameters : ParameterList = parameters
  member this.body : BlockStatement = body

type Type_Error(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members : IndexedVector<Declaration_ID> = members

type Declaration_MatchKind(node_id, node_type, members) =
  inherit Node(node_id, node_type)
  member this.members : IndexedVector<Declaration_ID> = members

type PropertyValue(node_id, node_type) =
  inherit Node(node_id, node_type)

type ExpressionValue(node_id, node_type, expression) =
  inherit PropertyValue(node_id, node_type)
  member this.expression : Expression = expression

type ExpressionListValue(node_id, node_type, expressions) =
  inherit PropertyValue(node_id, node_type)
  member this.expressions : Vector<Expression> = expressions

type ActionListElement(node_id, node_type, annotations, expression) =
  inherit Node(node_id, node_type)
  interface IDeclaration
  member this.annotations : Annotations = annotations
  member this.expression : Expression = expression

type ActionList(node_id, node_type, actionList) =
  inherit PropertyValue(node_id, node_type)
  member this.actionList : IndexedVector<ActionListElement> = actionList

type KeyElement(node_id, node_type, annotations, expression, matchType) =
  inherit Node(node_id, node_type)
  member this.annotations : Annotations = annotations
  member this.expression : Expression = expression
  member this.matchType : PathExpression = matchType

type Key(node_id, node_type, keyElements) =
  inherit PropertyValue(node_id, node_type)
  member this.keyElements : Vector<KeyElement> = keyElements

type Property(node_id, node_type, name, declid, annotations, value, isConstant) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.value : PropertyValue = value
  member this.isConstant : bool = isConstant

type TableProperties(node_id, node_type, properties) =
  inherit Node(node_id, node_type)
  member this.properties : IndexedVector<Property> = properties

type P4Table(node_id, node_type, name, declid, annotations, parameters, properties) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.parameters : ParameterList = parameters
  member this.properties : TableProperties = properties

type Declaration_Variable(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.type_ : Type = type_ // type
  member this.initializer : Expression = initializer // if != nullptr

type Declaration_Constant(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.type_ : Type = type_ // type
  member this.initializer : Expression = initializer

type Declaration_Instance(node_id, node_type, name, declid, annotations, type_, arguments, properties, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.type_ : Type = type_ // type
  member this.arguments : Vector<Expression> = arguments
  member this.properties : NameMap<Property> = properties
  member this.initializer : BlockStatement = initializer // if != nullptr

type P4Program(node_id, node_type, declarations) =
  inherit Node(node_id, node_type)
  member this.declarations : IndexedVector<Node> = declarations

type Statement(node_id, node_type) =
  inherit StatOrDecl(node_id, node_type)

type ExitStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

type ReturnStatement(node_id, node_type, expression) =
  inherit Statement(node_id, node_type)
  member this.expression : Expression = expression // if != nullptr

type EmptyStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

type AssignmentStatement(node_id, node_type, left, right) =
  inherit Statement(node_id, node_type)
  member this.left : Expression = left
  member this.right : Expression = right

type IfStatement(node_id, node_type, condition, ifTrue, ifFalse) =
  inherit Statement(node_id, node_type)
  member this.condition : Expression = condition
  member this.ifTrue : Statement = ifTrue
  member this.ifFalse : Statement = ifFalse // if != nullptr

type BlockStatement(node_id, node_type, annotations, components) =
  inherit Statement(node_id, node_type)
  member this.annotations : Annotations = annotations
  member this.components : IndexedVector<StatOrDecl> = components

type MethodCallStatement(node_id, node_type, methodCall) =
  inherit Statement(node_id, node_type)
  member this.methodCall : MethodCallExpression = methodCall

type SwitchCase(node_id, node_type, label, statement) =
  inherit Node(node_id, node_type)
  member this.label : Expression = label
  member this.statement : Statement = statement // if != nullptr

type SwitchStatement(node_id, node_type, expression, cases) =
  inherit Statement(node_id, node_type)
  member this.expression : Expression = expression
  member this.cases : Vector<SwitchCase> = cases

type Function(node_id, node_type, name, declid, type_, body) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ : Type_Method = type_ // type
  member this.body : BlockStatement = body

type Block(node_id, node_type, node, constantValue) =
  inherit Node(node_id, node_type)
  interface ICompileTimeValue
  member this.node : Node = node
  member this.constantValue : OrderedMap<Node, ICompileTimeValue> = constantValue

type TableBlock(node_id, node_type, node, constantValue, container) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.container : P4Table = container

type InstantiatedBlock(node_id, node_type, node, constantValue, instanceType) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.instanceType : Type = instanceType

type ParserBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container : P4Parser = container

type ControlBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container : P4Control = container

type PackageBlock(node_id, node_type, node, constantValue, instanceType, type_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.type_ : Type_Package = type_ // type

type ExternBlock(node_id, node_type, node, constantValue, instanceType, type_, constructor_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.type_ : Type_Extern = type_ // type
  member this.constructor_ : Method = constructor_ // constructor

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
  member this.type_name : ID = type_name
  member this.name : ID = name
  member this.annotations : Annotations = annotations
  member this.type_ : Type_StructLike = type_ // type; if != nullptr

type Header(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

type HeaderStack(node_id, node_type, type_name, name, annotations, type_, size) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)
  member this.size : int = size

type Metadata(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

type HeaderRef(node_id, node_type, type_) =
  inherit Expression(node_id, node_type, type_)

type ConcreteHeaderRef(node_id, node_type, type_name, name, annotations, type_, ref) =
  inherit HeaderRef(node_id, node_type, type_)
  member this.ref : HeaderOrMetadata = ref

type HeaderStackItemRef(node_id, node_type, type_, base_, index_) =
  inherit HeaderRef(node_id, node_type, type_)
  member this.base_ : Expression = base_ // sic
  member this.index_ : Expression = index_ // sic

type NamedRef(node_id, node_type, type_, name) =
  inherit Expression(node_id, node_type, type_)
  member this.name : ID = name

type If(node_id, node_type, type_, pred, ifTrue, ifFalse) =
  inherit Expression(node_id, node_type, type_)
  member this.pred : Expression = pred
  member this.ifTrue : Vector<Expression> = ifTrue // if != nullptr
  member this.ifFalse : Vector<Expression> = ifFalse // if != nullptr

type NamedCond(node_id, node_type, type_, pred, ifTrue, ifFalse, name) =
  inherit If(node_id, node_type, type_, pred, ifTrue, ifFalse)
  member this.name : string = name

type Apply(node_id, node_type, type_, name, actions) =
  inherit Expression(node_id, node_type, type_)
  member this.name : ID = name
  member this.actions : NameMap<Vector<Expression>> = actions

type Primitive(node_id, node_type, type_, name, operands) =
  inherit Operation(node_id, node_type, type_)
  member this.name : string = name
  member this.operands : Vector<Expression> = operands

type FieldList(node_id, node_type, name, payload, annotations, fields) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.payload : bool = payload
  member this.annotations : Annotations = annotations
  member this.fields : Vector<Expression> = fields

type FieldListCalculation(node_id, node_type, name, input, algorithm, output_width, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.input : NameList = input // if != nullptr
  member this.algorithm : ID = algorithm
  member this.output_width : int = output_width
  member this.annotations : Annotations = annotations

type CalculatedField_update_or_verify =
  { update : bool;
    name : ID;
    cond : Expression; }
type CalculatedField(node_id, node_type, field, specs, annotations) =
  inherit Node(node_id, node_type)
  member this.field : Expression = field // if != nullptr
  member this.specs CalculatedField_update_or_verify array = specs
  member this.annotations : Annotations = annotations

type CaseEntry(node_id, node_type, values, action) =
  inherit Node(node_id, node_type)
  member this.values : (CaseEntry * Constant) array = values
  member this.action : ID = action

type V1Parser(node_id, node_type, name, stmts, select, cases, default_return, parse_error, drop, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.stmts : Vector<Expression> = stmts
  member this.select : Vector<Expression> = select // if != nullptr
  member this.cases : Vector<CaseEntry> = cases // if != nullptr
  member this.default_return : ID = default_return
  member this.parse_error : ID = parse_error
  member this.drop : bool = drop
  member this.annotations : Annotations = annotations

type ParserException(node_id, node_type) =
  inherit Node(node_id, node_type)

type Attached(node_id, node_type, name, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.annotations : Annotations = annotations

type Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.table : ID = table
  member this.direct : bool = direct
  member this.saturating : bool = saturating
  member this.instance_count : int = instance_count

type CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_) =
  inherit Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count)
  member this.type_ : CounterType = type_ // type

type Counter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_, max_width, min_width) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_)
  member this.max_width : int = max_width
  member this.min_width : int = min_width

type  Meter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_, result, pre_color, implementation) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_)
  member this.result : Expression = result // if != nullptr
  member this.pre_color : Expression = pre_color // if != nullptr
  member this.implementation : ID = implementation

type Register(node_id, node_type, name, annotations, table, direct, saturating, instance_count, layout, width, signed_) =
  inherit Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count)
  member this.layout : ID = layout
  member this.width : int = width
  member this.signed_ : bool = signed_ // sic

type PrimitiveAction(node_id, node_type) =
  inherit Node(node_id, node_type)

type NameList(node_id, node_type, names) =
  inherit Node(node_id, node_type)
  member this.names : ID array = names

type ActionArg(node_id, node_type, type_, action_name, name, read, write) =
  inherit Expression(node_id, node_type, type_)
  member this.action_name : string = action_name
  member this.name : ID = name
  member this.read : bool = read
  member this.write : bool = write

type ActionFunction(node_id, node_type, name, action, args, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.action : Vector<Primitive> = action
  member this.args : ActionArg array = args
  member this.annotations : Annotations = annotations

type ActionProfile(node_id, node_type, name, annotations, selector, actions, size) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.selector : ID = selector
  member this.actions : ID array = actions
  member this.size : int = size

type ActionSelector(node_id, node_type, name, annotations, key, mode, type_) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.key : ID = key
  member this.mode : ID = mode
  member this.type_ : ID = type_ // type

type V1Table(node_id, node_type, name, reads, reads_types, min_size, max_size, size, action_profile, actions, default_action, default_action_args, properties, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.reads : Expression array = reads // if != nullptr
  member this.reads_types : ID array = reads_types
  member this.min_size : int = min_size
  member this.max_size : int = max_size
  member this.size : int = size
  member this.action_profile : ID = action_profile
  member this.actions : ID array = actions
  member this.default_action : ID = default_action
  member this.default_action_args : Vector<Expression> = default_action_args // if != nullptr
  member this.properties : TableProperties = properties
  member this.annotations : Annotations = annotations

type V1Control(node_id, node_type, name, code, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.code : Vector<Expression> = code
  member this.annotations : Annotations = annotations

type Attribute(node_id, node_type, name, declid, type_, locals, optional) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.type_ : Type = type_ // type; if != nullptr
  member this.locals : NameList = locals // if != nullptr
  member this.optional : bool = optional

type V1Program(node_id, node_type, scope) =
  inherit Node(node_id, node_type)
  member this.scope : NameMap<Node> = scope // multimap

type v1HeaderType(node_id, node_type, name, as_metadata, as_header) = // sic
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.as_metadata : Type_Struct = as_metadata
  member this.as_header : Type_Header = as_header // if != nullptr

type IntMod(node_id, node_type, type_, expr, width) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  member this.width : uint32 = width
