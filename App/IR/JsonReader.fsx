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

