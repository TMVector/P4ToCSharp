(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for code parsed by p4c and dumped as JSON.
*)

#if !INTERACTIVE
module P4ToCSharp.App.IR
#endif

#if INTERACTIVE
#r "../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#r "../../packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll" 
#endif
open Newtonsoft.Json

type IDictionary<'K,'V> = System.Collections.Generic.IDictionary<'K,'V>
type Dictionary<'K,'V> = System.Collections.Generic.Dictionary<'K,'V>
type ILookup<'K,'V> = System.Linq.ILookup<'K,'V>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>

// Original IR uses a struct with original name and source info, but only name is serialised
type ID = string

// Marker interfaces
type INode = interface end
type IDeclaration = inherit INode
type IContainer = inherit IDeclaration
type ICompileTimeValue = inherit INode

// FIXME Makeshift map types
type OrderedMap<'K, 'V> = ('K * 'V) array
type MultiMap<'K, 'V> = ILookup<'K, 'V>
type UnorderedMap<'K, 'V> = IDictionary<'K, 'V>
type vector<'T> = 'T array
type OrderedMultiMap<'K, 'V> = ('K * 'V list) array

let (@) t idx =
    match t.GetType().GetProperty(sprintf "Item%d" idx) with
    | null -> invalidArg "idx" "invalid index"
    | p -> p.GetValue(t, null) |> unbox
type OrderedMapConverter() =
  inherit JsonConverter()
  static member private getPairTypes (t:System.Type) =
    if t.IsArray && Reflection.FSharpType.IsTuple(t.GetElementType()) then
      let elt = t.GetElementType()
      let ft = elt.GetProperty("Item1").PropertyType
      let st = elt.GetProperty("Item2").PropertyType
      Option.Some (elt, ft, st)
    else
      Option.None
  override this.CanConvert(objectType) =
    OrderedMapConverter.getPairTypes objectType <> Option.None
  override this.WriteJson(writer, value, serialiser) =
    let arr = value :?> System.Array
    let d = new System.Collections.Specialized.OrderedDictionary()
    for o in arr do
      d.Add(o@1, o@2)
    serialiser.Serialize(writer, d)
    ()
  override this.ReadJson(reader, objectType, existingValue, serialiser) =
    let types = OrderedMapConverter.getPairTypes objectType
    let (elt, keyT, valT) =
        match types with
        | Option.Some t -> t
        | Option.None -> failwith (sprintf "Cannot convert to type %s" objectType.Name)
    let entries =
      seq {
        if reader.TokenType = JsonToken.StartObject then
          while reader.Read() && reader.TokenType = JsonToken.PropertyName do // Should end when reads EndObject
            let key = reader.Value // string for now FIXME convert to correct type
            let value = serialiser.Deserialize(reader, valT)
            yield (key, value)
      } |> Seq.toArray
    let arr = System.Array.CreateInstance(elt, entries.Length)
    for (i, (k,v)) in entries |> Seq.indexed do
      arr.SetValue(Microsoft.FSharp.Reflection.FSharpValue.MakeTuple([|k; v|], elt), i)
    arr :> obj
    //entries :> obj


type Node(node_id, node_type) =
  interface INode
  member this.Node_ID : int = node_id
  member this.Node_Type : string = node_type

type Vector<'T>(node_id, node_type, vec) =
  inherit Node(node_id, node_type)
  member this.vec : vector<'T> = vec // value is json list

[<Sealed>]
type IndexedVector<'T>(node_id, node_type, vec, declarations) =
  inherit Vector<'T>(node_id, node_type, vec)
  member this.declarations : OrderedMap<string, IDeclaration> = declarations // value is json dictionary

[<Sealed>]
type NameMap<'T>(node_id, node_type, symbols) =
  inherit Node(node_id, node_type)
  // FIXME We are using one datastructure instead of parameterising like in the C++.
  //       This has the problem of not enforcing single value when it's not supposed to be a multimap...
  member this.symbols : OrderedMultiMap<string, 'T> = symbols // value is json dictionary

type Type(node_id, node_type) =
  inherit Node(node_id, node_type)

type Type_Base(node_id, node_type) =
  inherit Type(node_id, node_type)

[<Sealed>]
type Type_Unknown(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

type StatOrDecl(node_id, node_type) =
  inherit Node(node_id, node_type)

type Declaration(node_id, node_type, name, declid) =
  inherit StatOrDecl(node_id, node_type)
  interface IDeclaration
  member this.name : ID = name
  member this.declid : int = declid

[<AbstractClass>]
type Type_Declaration(node_id, node_type, name, declid) =
  inherit Type(node_id, node_type)
  interface IDeclaration
  member this.name : ID = name
  member this.declid : int = declid

[<AbstractClass>]
type Expression(node_id, node_type, type_) =
  inherit Node(node_id, node_type)
  member this.type_ : Type = type_

[<AbstractClass>]
type Operation(node_id, node_type, type_) =
  inherit Expression(node_id, node_type, type_)

[<Sealed>]
type Path(node_id, node_type, name, absolute) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.absolute : bool = absolute

[<Sealed>]
type Annotation(node_id, node_type, name, expr) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.expr : Vector<Expression> = expr

[<Sealed>]
type Annotations(node_id, node_type, annotations) =
  inherit Node(node_id, node_type)
  member this.annotations : Vector<Annotation> = annotations

type Direction = None | In | Out | InOut with
  static member toString d =
    match d with
    | Direction.None -> ""
    | Direction.In -> "in"
    | Direction.Out -> "out"
    | Direction.InOut -> "inout"
  static member parse s =
    match s with
    | "" -> Direction.None
    | "in" -> Direction.In
    | "out" -> Direction.Out
    | "inout" -> Direction.InOut
    | _ -> failwith "Couldn't parse Direction"
type DirectionJsonConverter() =
  inherit JsonConverter()
  override this.CanConvert(objectType) = objectType = typeof<Direction>
  override this.ReadJson(reader, objectType, existingValue, serialiser) =
    Direction.parse <| serialiser.Deserialize<string>(reader) :> obj
  override this.WriteJson(writer, value, serialiser) =
    writer.WriteValue(Direction.toString(value :?> Direction))

[<Sealed>]
type Type_Type(node_id, node_type, type_) =
  inherit Type(node_id, node_type)
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type

[<Sealed>]
type Type_Boolean(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_State(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Bits(node_id, node_type, size, isSigned) =
  inherit Type_Base(node_id, node_type)
  member this.size : int = size
  member this.isSigned : bool = isSigned

[<Sealed>]
type Type_Varbits(node_id, node_type, size) =
  inherit Type_Base(node_id, node_type)
  member this.size : int = size

[<Sealed>]
type Parameter(node_id, node_type, name, declid, annotations, direction, type_) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.direction : Direction = direction
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type

[<Sealed>]
type ParameterList(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters : IndexedVector<Parameter> = parameters

[<Sealed>]
type Type_Var(node_id, node_type, name, declid) =
  inherit Type_Declaration(node_id, node_type, name, declid)

[<Sealed>]
type Type_InfInt(node_id, node_type, declid) =
  inherit Type(node_id, node_type)
  member this.declid : int = declid

[<Sealed>]
type Type_Dontcare(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Void(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_MatchKind(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type TypeParameters(node_id, node_type, parameters) =
  inherit Node(node_id, node_type)
  member this.parameters : IndexedVector<Type_Var> = parameters

[<Sealed>]
type StructField(node_id, node_type, name, declid, annotations, type_) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type

[<AbstractClass>]
type Type_StructLike(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.fields : IndexedVector<StructField> = fields

[<Sealed>]
type Type_Struct(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

[<Sealed>]
type Type_Union(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

[<Sealed>]
type Type_Header(node_id, node_type, name, declid, annotations, fields) =
  inherit Type_StructLike(node_id, node_type, name, declid, annotations, fields)

[<Sealed>]
type Type_Set(node_id, node_type, elementType) =
  inherit Type(node_id, node_type)
  member this.elementType : Type = elementType

[<Sealed>]
type Type_Tuple(node_id, node_type, components) =
  inherit Type(node_id, node_type)
  member this.components : Vector<Type> = components

[<AbstractClass>]
type Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.typeParameters : TypeParameters = typeParameters

[<Sealed>]
type Type_Package(node_id, node_type, name, declid, annotations, typeParameters, constructorParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  interface IContainer
  member this.constructorParams : ParameterList = constructorParams

[<Sealed>]
type Type_Parser(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams : ParameterList = applyParams

[<Sealed>]
type Type_Control(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
  inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
  member this.applyParams : ParameterList = applyParams

[<Sealed>]
type Type_Name(node_id, node_type, path) =
  inherit Type(node_id, node_type)
  member this.path : Path = path

[<Sealed>]
type Type_Stack(node_id, node_type, elementType, size) =
  inherit Type(node_id, node_type)
  member this.elementType : Type = elementType
  member this.size : Expression = size

[<Sealed>]
type Type_Specialized(node_id, node_type, baseType, arguments) =
  inherit Type(node_id, node_type)
  member this.baseType : Type_Name = baseType
  member this.arguments : Vector<Type> = arguments

[<Sealed>]
type Type_SpecializedCanonical(node_id, node_type, baseType, arguments, substituted) =
  inherit Type(node_id, node_type)
  member this.baseType : Type = baseType
  member this.arguments : Vector<Type> = arguments
  member this.substituted : Type = substituted

[<Sealed>]
type Declaration_ID(node_id, node_type, name, declid) =
  inherit Declaration(node_id, node_type, name, declid)
  interface ICompileTimeValue

[<Sealed>]
type Type_String(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Enum(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members : IndexedVector<Declaration_ID> = members

[<AbstractClass>]
type PropertyValue(node_id, node_type) =
  inherit Node(node_id, node_type)

[<Sealed>]
type Property(node_id, node_type, name, declid, annotations, value, isConstant) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.value : PropertyValue = value
  member this.isConstant : bool = isConstant

[<Sealed>]
type TableProperties(node_id, node_type, properties) =
  inherit Node(node_id, node_type)
  member this.properties : IndexedVector<Property> = properties

[<Sealed>]
type P4Table(node_id, node_type, name, declid, annotations, parameters, properties) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.parameters : ParameterList = parameters
  member this.properties : TableProperties = properties

[<Sealed>]
type Type_Table(node_id, node_type, table) =
  inherit Type(node_id, node_type)
  member this.table : P4Table = table

[<Sealed>]
type ActionListElement(node_id, node_type, annotations, expression) =
  inherit Node(node_id, node_type)
  interface IDeclaration
  member this.annotations : Annotations = annotations
  member this.expression : Expression = expression

[<Sealed>]
type ActionList(node_id, node_type, actionList) =
  inherit PropertyValue(node_id, node_type)
  member this.actionList : IndexedVector<ActionListElement> = actionList

[<Sealed>]
type Type_ActionEnum(node_id, node_type, actionList) =
  inherit Type(node_id, node_type)
  member this.actionList : ActionList = actionList

[<AbstractClass>]
type Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type(node_id, node_type)
  member this.typeParameters : TypeParameters = typeParameters
  member this.returnType : Type = returnType // if != nullptr
  member this.parameters : ParameterList = parameters

[<Sealed>]
type Type_Method(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

[<Sealed>]
type ArgumentInfo(node_id, node_type, leftValue, compileTimeConstant, type_) =
  inherit Node(node_id, node_type)
  member this.leftValue : bool = leftValue
  member this.compileTimeConstant : bool = compileTimeConstant
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type

[<Sealed>]
type Type_MethodCall(node_id, node_type, typeArguments, returnType, arguments) =
  inherit Type(node_id, node_type)
  member this.typeParameters : Vector<Type> = typeArguments
  member this.returnType : Type_Var = returnType
  member this.arguments : Vector<ArgumentInfo> = arguments

[<Sealed>]
type Type_Action(node_id, node_type, typeParameters, returnType, parameters) =
  inherit Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters)

[<Sealed>]
type Method(node_id, node_type, name, declid, type_, isAbstract, annotations) =
  inherit Declaration(node_id, node_type, name, declid)
  [<JsonProperty("type")>]
  member this.type_ : Type_Method = type_ // type
  member this.isAbstract : bool = isAbstract
  member this.annotations : Annotations = annotations

[<Sealed>]
type Type_Typedef(node_id, node_type, name, declid, annotations, type_) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type

[<Sealed>]
type NameList(node_id, node_type, names) =
  inherit Node(node_id, node_type)
  member this.names : vector<ID> = names

[<Sealed>]
type Attribute(node_id, node_type, name, declid, type_, locals, optional) =
  inherit Declaration(node_id, node_type, name, declid)
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type; if != nullptr
  member this.locals : NameList = locals // if != nullptr
  member this.optional : bool = optional

[<Sealed>]
type Type_Extern(node_id, node_type, name, declid, typeParameters, methods, attributes, annotations) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.typeParameters : TypeParameters = typeParameters
  member this.methods : Vector<Method> = methods
  member this.attributes : NameMap<Attribute> = attributes // ordered_map
  member this.annotations : Annotations = annotations

[<AbstractClass>]
type Operation_Unary(node_id, node_type, type_, expr) =
  inherit Operation(node_id, node_type, type_)
  member this.expr : Expression = expr

[<Sealed>]
type Neg(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

[<Sealed>]
type Cmpl(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

[<Sealed>]
type LNot(node_id, node_type, type_, expr) =
  inherit Operation_Unary(node_id, node_type, type_, expr)

[<AbstractClass>]
type Operation_Binary(node_id, node_type, type_, left, right) =
  inherit Operation(node_id, node_type, type_)
  member this.left : Expression = left
  member this.right : Expression = right

[<AbstractClass>]
type Operation_Ternary(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation(node_id, node_type, type_)
  member this.e0 : Expression = e0
  member this.e1 : Expression = e1
  member this.e2 : Expression = e2

[<AbstractClass>]
type Operation_Relation(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Mul(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Div(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Mod(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Add(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Sub(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Shl(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Shr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Equ(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Neq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Lss(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Leq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Grt(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Geq(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type BAnd(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type BOr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type BXor(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type LAnd(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type LOr(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<AbstractClass>]
type Literal(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)
  interface ICompileTimeValue

[<Sealed>]
type Constant(node_id, node_type, type_, value, base_) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : bigint = value
  [<JsonProperty("base")>]
  member this.base_ : uint32 = base_ // base

[<Sealed>]
type BoolLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : bool = value

[<Sealed>]
type StringLiteral(node_id, node_type, type_, value) = 
  inherit Literal(node_id, node_type, type_)
  member this.value : string = value

[<Sealed>]
type PathExpression(node_id, node_type, type_, path) = 
  inherit Expression(node_id, node_type, type_)
  member this.path : Path = path

[<Sealed>]
type TypeNameExpression(node_id, node_type, type_, typeName) = 
  inherit Expression(node_id, node_type, type_)
  member this.typeName : Type_Name = typeName

[<Sealed>]
type Slice(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation_Ternary(node_id, node_type, type_, e0, e1, e2)

[<Sealed>]
type Member(node_id, node_type, type_, expr, member_) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  [<JsonProperty("member")>]
  member this.member_ : ID = member_ // member

[<Sealed>]
type Concat(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type ArrayIndex(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Range(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Mask(node_id, node_type, type_, left, right) =
  inherit Operation_Binary(node_id, node_type, type_, left, right)

[<Sealed>]
type Mux(node_id, node_type, type_, e0, e1, e2) =
  inherit Operation_Ternary(node_id, node_type, type_, e0, e1, e2)

[<Sealed>]
type DefaultExpression(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)

[<Sealed>]
type This(node_id, node_type, type_) = 
  inherit Expression(node_id, node_type, type_)

[<Sealed>]
type Cast(node_id, node_type, destType, expr) =
  inherit Operation_Unary(node_id, node_type, destType, expr)
  member this.destType : Type = destType

[<Sealed>]
type SelectCase(node_id, node_type, keyset, state) =
  inherit Node(node_id, node_type)
  member this.keyset : Expression = keyset
  member this.state : PathExpression = state

[<Sealed>]
type ListExpression(node_id, node_type, type_, components) = 
  inherit Expression(node_id, node_type, type_)
  member this.components : Vector<Expression> = components

[<Sealed>]
type SelectExpression(node_id, node_type, type_, select, selectCases) = 
  inherit Expression(node_id, node_type, type_)
  member this.select : ListExpression = select
  member this.selectCases : Vector<SelectCase> = selectCases

[<Sealed>]
type MethodCallExpression(node_id, node_type, type_, method_, typeArguments, arguments) = 
  inherit Expression(node_id, node_type, type_)
  [<JsonProperty("method")>]
  member this.method_ : Expression = method_ // method
  member this.typeArguments : Vector<Type> = typeArguments
  member this.arguments : Vector<Expression> = arguments

[<Sealed>]
type ConstructorCallExpression(node_id, node_type, type_, arguments) = 
  inherit Expression(node_id, node_type, type_)
  member this.constructedType : Type = this.type_
  member this.arguments : Vector<Expression> = arguments

[<Sealed>]
type ParserState(node_id, node_type, name, declid, annotations, components, selectExpression) = 
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.components : IndexedVector<StatOrDecl> = components
  member this.selectExpression : Expression = selectExpression // if != nullptr

[<Sealed>]
type P4Parser(node_id, node_type, name, declid, type_, constructorParams, parserLocals, states) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  interface IContainer
  [<JsonProperty("type")>]
  member this.type_ : Type_Parser = type_ // type
  member this.constructorParams : ParameterList = constructorParams
  member this.parserLocals : IndexedVector<Declaration> = parserLocals
  member this.states : IndexedVector<ParserState> = states

[<AbstractClass>]
type Statement(node_id, node_type) =
  inherit StatOrDecl(node_id, node_type)

[<Sealed>]
type BlockStatement(node_id, node_type, annotations, components) =
  inherit Statement(node_id, node_type)
  member this.annotations : Annotations = annotations
  member this.components : IndexedVector<StatOrDecl> = components

[<Sealed>]
type P4Control(node_id, node_type, name, declid, type_, constructorParams, controlLocals, body) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  interface IContainer
  [<JsonProperty("type")>]
  member this.type_ : Type_Control = type_ // type
  member this.constructorParams : ParameterList = constructorParams
  member this.controlLocals : IndexedVector<Declaration> = controlLocals
  member this.body : BlockStatement = body

[<Sealed>]
type P4Action(node_id, node_type, name, declid, annotations, parameters, body) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  member this.parameters : ParameterList = parameters
  member this.body : BlockStatement = body

[<Sealed>]
type Type_Error(node_id, node_type, name, declid, members) =
  inherit Type_Declaration(node_id, node_type, name, declid)
  member this.members : IndexedVector<Declaration_ID> = members

[<Sealed>]
type Declaration_MatchKind(node_id, node_type, members) =
  inherit Node(node_id, node_type)
  member this.members : IndexedVector<Declaration_ID> = members

[<Sealed>]
type ExpressionValue(node_id, node_type, expression) =
  inherit PropertyValue(node_id, node_type)
  member this.expression : Expression = expression

[<Sealed>]
type ExpressionListValue(node_id, node_type, expressions) =
  inherit PropertyValue(node_id, node_type)
  member this.expressions : Vector<Expression> = expressions

[<Sealed>]
type KeyElement(node_id, node_type, annotations, expression, matchType) =
  inherit Node(node_id, node_type)
  member this.annotations : Annotations = annotations
  member this.expression : Expression = expression
  member this.matchType : PathExpression = matchType

[<Sealed>]
type Key(node_id, node_type, keyElements) =
  inherit PropertyValue(node_id, node_type)
  member this.keyElements : Vector<KeyElement> = keyElements

[<Sealed>]
type Declaration_Variable(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type
  member this.initializer : Expression = initializer // if != nullptr

[<Sealed>]
type Declaration_Constant(node_id, node_type, name, declid, annotations, type_, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type
  member this.initializer : Expression = initializer

[<Sealed>]
type Declaration_Instance(node_id, node_type, name, declid, annotations, type_, arguments, properties, initializer) =
  inherit Declaration(node_id, node_type, name, declid)
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type = type_ // type
  member this.arguments : Vector<Expression> = arguments
  member this.properties : NameMap<Property> = properties
  member this.initializer : BlockStatement = initializer // if != nullptr

[<Sealed>]
type P4Program(node_id, node_type, declarations) =
  inherit Node(node_id, node_type)
  member this.declarations : IndexedVector<Node> = declarations

[<Sealed>]
type ExitStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

[<Sealed>]
type ReturnStatement(node_id, node_type, expression) =
  inherit Statement(node_id, node_type)
  member this.expression : Expression = expression // if != nullptr

[<Sealed>]
type EmptyStatement(node_id, node_type) =
  inherit Statement(node_id, node_type)

[<Sealed>]
type AssignmentStatement(node_id, node_type, left, right) =
  inherit Statement(node_id, node_type)
  member this.left : Expression = left
  member this.right : Expression = right

[<Sealed>]
type IfStatement(node_id, node_type, condition, ifTrue, ifFalse) =
  inherit Statement(node_id, node_type)
  member this.condition : Expression = condition
  member this.ifTrue : Statement = ifTrue
  member this.ifFalse : Statement = ifFalse // if != nullptr

[<Sealed>]
type MethodCallStatement(node_id, node_type, methodCall) =
  inherit Statement(node_id, node_type)
  member this.methodCall : MethodCallExpression = methodCall

[<Sealed>]
type SwitchCase(node_id, node_type, label, statement) =
  inherit Node(node_id, node_type)
  member this.label : Expression = label
  member this.statement : Statement = statement // if != nullptr

[<Sealed>]
type SwitchStatement(node_id, node_type, expression, cases) =
  inherit Statement(node_id, node_type)
  member this.expression : Expression = expression
  member this.cases : Vector<SwitchCase> = cases

[<Sealed>]
type Function(node_id, node_type, name, declid, type_, body) =
  inherit Declaration(node_id, node_type, name, declid)
  [<JsonProperty("type")>]
  member this.type_ : Type_Method = type_ // type
  member this.body : BlockStatement = body

[<AbstractClass>]
type Block(node_id, node_type, node, constantValue) =
  inherit Node(node_id, node_type)
  interface ICompileTimeValue
  member this.node : Node = node
  member this.constantValue : OrderedMap<Node, ICompileTimeValue> = constantValue

[<Sealed>]
type TableBlock(node_id, node_type, node, constantValue, container) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.container : P4Table = container

[<AbstractClass>]
type InstantiatedBlock(node_id, node_type, node, constantValue, instanceType) =
  inherit Block(node_id, node_type, node, constantValue)
  member this.instanceType : Type = instanceType

[<Sealed>]
type ParserBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container : P4Parser = container

[<Sealed>]
type ControlBlock(node_id, node_type, node, constantValue, instanceType, container) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  member this.container : P4Control = container

[<Sealed>]
type PackageBlock(node_id, node_type, node, constantValue, instanceType, type_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  [<JsonProperty("type")>]
  member this.type_ : Type_Package = type_ // type

[<Sealed>]
type ExternBlock(node_id, node_type, node, constantValue, instanceType, type_, constructor_) =
  inherit InstantiatedBlock(node_id, node_type, node, constantValue, instanceType)
  [<JsonProperty("type")>]
  member this.type_ : Type_Extern = type_ // type
  [<JsonProperty("constructor")>]
  member this.constructor_ : Method = constructor_ // constructor

[<Sealed>]
type ToplevelBlock(node_id, node_type, node, constantValue) =
  inherit Block(node_id, node_type, node, constantValue)

type CounterType = NONE | PACKETS | BYTES | BOTH

[<Sealed>]
type Type_Block(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Counter(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Expression(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_FieldListCalculation(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Meter(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_Register(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<Sealed>]
type Type_AnyTable(node_id, node_type) =
  inherit Type_Base(node_id, node_type)

[<AbstractClass>]
type HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_) =
  inherit Node(node_id, node_type)
  member this.type_name : ID = type_name
  member this.name : ID = name
  member this.annotations : Annotations = annotations
  [<JsonProperty("type")>]
  member this.type_ : Type_StructLike = type_ // type; if != nullptr

[<Sealed>]
type Header(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

[<Sealed>]
type HeaderStack(node_id, node_type, type_name, name, annotations, type_, size) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)
  member this.size : int = size

[<Sealed>]
type Metadata(node_id, node_type, type_name, name, annotations, type_) =
  inherit HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_)

[<AbstractClass>]
type HeaderRef(node_id, node_type, type_) =
  inherit Expression(node_id, node_type, type_)

[<Sealed>]
type ConcreteHeaderRef(node_id, node_type, type_name, name, annotations, type_, ref) =
  inherit HeaderRef(node_id, node_type, type_)
  member this.ref : HeaderOrMetadata = ref

[<Sealed>]
type HeaderStackItemRef(node_id, node_type, type_, base_, index_) =
  inherit HeaderRef(node_id, node_type, type_)
  member this.base_ : Expression = base_ // sic
  member this.index_ : Expression = index_ // sic

[<Sealed>]
type NamedRef(node_id, node_type, type_, name) =
  inherit Expression(node_id, node_type, type_)
  member this.name : ID = name

type If(node_id, node_type, type_, pred, ifTrue, ifFalse) =
  inherit Expression(node_id, node_type, type_)
  member this.pred : Expression = pred
  member this.ifTrue : Vector<Expression> = ifTrue // if != nullptr
  member this.ifFalse : Vector<Expression> = ifFalse // if != nullptr

[<Sealed>]
type NamedCond(node_id, node_type, type_, pred, ifTrue, ifFalse, name) =
  inherit If(node_id, node_type, type_, pred, ifTrue, ifFalse)
  member this.name : string = name

[<Sealed>]
type Apply(node_id, node_type, type_, name, actions) =
  inherit Expression(node_id, node_type, type_)
  member this.name : ID = name
  member this.actions : NameMap<Vector<Expression>> = actions

[<Sealed>]
type Primitive(node_id, node_type, type_, name, operands) =
  inherit Operation(node_id, node_type, type_)
  member this.name : string = name
  member this.operands : Vector<Expression> = operands

[<Sealed>]
type FieldList(node_id, node_type, name, payload, annotations, fields) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.payload : bool = payload
  member this.annotations : Annotations = annotations
  member this.fields : Vector<Expression> = fields

[<Sealed>]
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
[<Sealed>]
type CalculatedField(node_id, node_type, field, specs, annotations) =
  inherit Node(node_id, node_type)
  member this.field : Expression = field // if != nullptr
  member this.specs : vector<CalculatedField_update_or_verify> = specs
  member this.annotations : Annotations = annotations

[<Sealed>]
type CaseEntry(node_id, node_type, values, action) =
  inherit Node(node_id, node_type)
  member this.values : vector<CaseEntry * Constant> = values
  member this.action : ID = action

[<Sealed>]
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

[<Sealed>]
type ParserException(node_id, node_type) =
  inherit Node(node_id, node_type)

[<AbstractClass>]
type Attached(node_id, node_type, name, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.annotations : Annotations = annotations

[<AbstractClass>]
type Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.table : ID = table
  member this.direct : bool = direct
  member this.saturating : bool = saturating
  member this.instance_count : int = instance_count

[<AbstractClass>]
type CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_) =
  inherit Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count)
  [<JsonProperty("type")>]
  member this.type_ : CounterType = type_ // type

[<Sealed>]
type Counter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_, max_width, min_width) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_)
  member this.max_width : int = max_width
  member this.min_width : int = min_width

[<Sealed>]
type  Meter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_, result, pre_color, implementation) =
  inherit CounterOrMeter(node_id, node_type, name, annotations, table, direct, saturating, instance_count, type_)
  member this.result : Expression = result // if != nullptr
  member this.pre_color : Expression = pre_color // if != nullptr
  member this.implementation : ID = implementation

[<Sealed>]
type Register(node_id, node_type, name, annotations, table, direct, saturating, instance_count, layout, width, signed_) =
  inherit Stateful(node_id, node_type, name, annotations, table, direct, saturating, instance_count)
  member this.layout : ID = layout
  member this.width : int = width
  member this.signed_ : bool = signed_ // sic

[<Sealed>]
type PrimitiveAction(node_id, node_type) =
  inherit Node(node_id, node_type)

[<Sealed>]
type ActionArg(node_id, node_type, type_, action_name, name, read, write) =
  inherit Expression(node_id, node_type, type_)
  member this.action_name : string = action_name
  member this.name : ID = name
  member this.read : bool = read
  member this.write : bool = write

[<Sealed>]
type ActionFunction(node_id, node_type, name, action, args, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.action : Vector<Primitive> = action
  member this.args : vector<ActionArg> = args
  member this.annotations : Annotations = annotations

[<Sealed>]
type ActionProfile(node_id, node_type, name, annotations, selector, actions, size) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.selector : ID = selector
  member this.actions : vector<ID> = actions
  member this.size : int = size

[<Sealed>]
type ActionSelector(node_id, node_type, name, annotations, key, mode, type_) =
  inherit Attached(node_id, node_type, name, annotations)
  member this.key : ID = key
  member this.mode : ID = mode
  [<JsonProperty("type")>]
  member this.type_ : ID = type_ // type

[<Sealed>]
type V1Table(node_id, node_type, name, reads, reads_types, min_size, max_size, size, action_profile, actions,
              default_action, default_action_args, properties, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.reads : Vector<Expression> = reads // if != nullptr
  member this.reads_types : vector<ID> = reads_types
  member this.min_size : int = min_size
  member this.max_size : int = max_size
  member this.size : int = size
  member this.action_profile : ID = action_profile
  member this.actions : vector<ID> = actions
  member this.default_action : ID = default_action
  member this.default_action_args : Vector<Expression> = default_action_args // if != nullptr
  member this.properties : TableProperties = properties
  member this.annotations : Annotations = annotations

[<Sealed>]
type V1Control(node_id, node_type, name, code, annotations) =
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.code : Vector<Expression> = code
  member this.annotations : Annotations = annotations

[<Sealed>]
type V1Program(node_id, node_type, scope) =
  inherit Node(node_id, node_type)
  member this.scope : NameMap<Node> = scope // multimap

[<Sealed>]
type v1HeaderType(node_id, node_type, name, as_metadata, as_header) = // sic
  inherit Node(node_id, node_type)
  member this.name : ID = name
  member this.as_metadata : Type_Struct = as_metadata
  member this.as_header : Type_Header = as_header // if != nullptr

[<Sealed>]
type IntMod(node_id, node_type, type_, expr, width) =
  inherit Operation_Unary(node_id, node_type, type_, expr)
  member this.width : uint32 = width

let TypeNames =
  [|
    ("Node", typeof<Node>);
    ("Vector", typeof<Vector<_>>.GetGenericTypeDefinition());
    ("IndexedVector", typeof<IndexedVector<_>>.GetGenericTypeDefinition());
    ("NameMap", typeof<NameMap<_>>.GetGenericTypeDefinition());
    ("Type", typeof<Type>);
    ("Type_Base", typeof<Type_Base>);
    ("Type_Unknown", typeof<Type_Unknown>);
    ("StatOrDecl", typeof<StatOrDecl>);
    ("Declaration", typeof<Declaration>);
    ("Type_Declaration", typeof<Type_Declaration>);
    ("Expression", typeof<Expression>);
    ("Operation", typeof<Operation>);
    ("Path", typeof<Path>);
    ("Annotation", typeof<Annotation>);
    ("Annotations", typeof<Annotations>);
    ("Direction", typeof<Direction>);
    ("Type_Type", typeof<Type_Type>);
    ("Type_Boolean", typeof<Type_Boolean>);
    ("Type_State", typeof<Type_State>);
    ("Type_Bits", typeof<Type_Bits>);
    ("Type_Varbits", typeof<Type_Varbits>);
    ("Parameter", typeof<Parameter>);
    ("ParameterList", typeof<ParameterList>);
    ("Type_Var", typeof<Type_Var>);
    ("Type_InfInt", typeof<Type_InfInt>);
    ("Type_Dontcare", typeof<Type_Dontcare>);
    ("Type_Void", typeof<Type_Void>);
    ("Type_MatchKind", typeof<Type_MatchKind>);
    ("TypeParameters", typeof<TypeParameters>);
    ("StructField", typeof<StructField>);
    ("Type_StructLike", typeof<Type_StructLike>);
    ("Type_Struct", typeof<Type_Struct>);
    ("Type_Union", typeof<Type_Union>);
    ("Type_Header", typeof<Type_Header>);
    ("Type_Set", typeof<Type_Set>);
    ("Type_Tuple", typeof<Type_Tuple>);
    ("Type_ArchBlock", typeof<Type_ArchBlock>);
    ("Type_Package", typeof<Type_Package>);
    ("Type_Parser", typeof<Type_Parser>);
    ("Type_Control", typeof<Type_Control>);
    ("Type_Name", typeof<Type_Name>);
    ("Type_Stack", typeof<Type_Stack>);
    ("Type_Specialized", typeof<Type_Specialized>);
    ("Type_SpecializedCanonical", typeof<Type_SpecializedCanonical>);
    ("Declaration_ID", typeof<Declaration_ID>);
    ("Type_String", typeof<Type_String>);
    ("Type_Enum", typeof<Type_Enum>);
    ("PropertyValue", typeof<PropertyValue>);
    ("Property", typeof<Property>);
    ("TableProperties", typeof<TableProperties>);
    ("P4Table", typeof<P4Table>);
    ("Type_Table", typeof<Type_Table>);
    ("ActionListElement", typeof<ActionListElement>);
    ("ActionList", typeof<ActionList>);
    ("Type_ActionEnum", typeof<Type_ActionEnum>);
    ("Type_MethodBase", typeof<Type_MethodBase>);
    ("Type_Method", typeof<Type_Method>);
    ("ArgumentInfo", typeof<ArgumentInfo>);
    ("Type_MethodCall", typeof<Type_MethodCall>);
    ("Type_Action", typeof<Type_Action>);
    ("Method", typeof<Method>);
    ("Type_Typedef", typeof<Type_Typedef>);
    ("NameList", typeof<NameList>);
    ("Attribute", typeof<Attribute>);
    ("Type_Extern", typeof<Type_Extern>);
    ("Operation_Unary", typeof<Operation_Unary>);
    ("Neg", typeof<Neg>);
    ("Cmpl", typeof<Cmpl>);
    ("LNot", typeof<LNot>);
    ("Operation_Binary", typeof<Operation_Binary>);
    ("Operation_Ternary", typeof<Operation_Ternary>);
    ("Operation_Relation", typeof<Operation_Relation>);
    ("Mul", typeof<Mul>);
    ("Div", typeof<Div>);
    ("Mod", typeof<Mod>);
    ("Add", typeof<Add>);
    ("Sub", typeof<Sub>);
    ("Shl", typeof<Shl>);
    ("Shr", typeof<Shr>);
    ("Equ", typeof<Equ>);
    ("Neq", typeof<Neq>);
    ("Lss", typeof<Lss>);
    ("Leq", typeof<Leq>);
    ("Grt", typeof<Grt>);
    ("Geq", typeof<Geq>);
    ("BAnd", typeof<BAnd>);
    ("BOr", typeof<BOr>);
    ("BXor", typeof<BXor>);
    ("LAnd", typeof<LAnd>);
    ("LOr", typeof<LOr>);
    ("Literal", typeof<Literal>);
    ("Constant", typeof<Constant>);
    ("BoolLiteral", typeof<BoolLiteral>);
    ("StringLiteral", typeof<StringLiteral>);
    ("PathExpression", typeof<PathExpression>);
    ("TypeNameExpression", typeof<TypeNameExpression>);
    ("Slice", typeof<Slice>);
    ("Member", typeof<Member>);
    ("Concat", typeof<Concat>);
    ("ArrayIndex", typeof<ArrayIndex>);
    ("Range", typeof<Range>);
    ("Mask", typeof<Mask>);
    ("Mux", typeof<Mux>);
    ("DefaultExpression", typeof<DefaultExpression>);
    ("This", typeof<This>);
    ("Cast", typeof<Cast>);
    ("SelectCase", typeof<SelectCase>);
    ("ListExpression", typeof<ListExpression>);
    ("SelectExpression", typeof<SelectExpression>);
    ("MethodCallExpression", typeof<MethodCallExpression>);
    ("ConstructorCallExpression", typeof<ConstructorCallExpression>);
    ("ParserState", typeof<ParserState>);
    ("P4Parser", typeof<P4Parser>);
    ("Statement", typeof<Statement>);
    ("BlockStatement", typeof<BlockStatement>);
    ("P4Control", typeof<P4Control>);
    ("P4Action", typeof<P4Action>);
    ("Type_Error", typeof<Type_Error>);
    ("Declaration_MatchKind", typeof<Declaration_MatchKind>);
    ("ExpressionValue", typeof<ExpressionValue>);
    ("ExpressionListValue", typeof<ExpressionListValue>);
    ("KeyElement", typeof<KeyElement>);
    ("Key", typeof<Key>);
    ("Declaration_Variable", typeof<Declaration_Variable>);
    ("Declaration_Constant", typeof<Declaration_Constant>);
    ("Declaration_Instance", typeof<Declaration_Instance>);
    ("P4Program", typeof<P4Program>);
    ("ExitStatement", typeof<ExitStatement>);
    ("ReturnStatement", typeof<ReturnStatement>);
    ("EmptyStatement", typeof<EmptyStatement>);
    ("AssignmentStatement", typeof<AssignmentStatement>);
    ("IfStatement", typeof<IfStatement>);
    ("MethodCallStatement", typeof<MethodCallStatement>);
    ("SwitchCase", typeof<SwitchCase>);
    ("SwitchStatement", typeof<SwitchStatement>);
    ("Function", typeof<Function>);
    ("Block", typeof<Block>);
    ("TableBlock", typeof<TableBlock>);
    ("InstantiatedBlock", typeof<InstantiatedBlock>);
    ("ParserBlock", typeof<ParserBlock>);
    ("ControlBlock", typeof<ControlBlock>);
    ("PackageBlock", typeof<PackageBlock>);
    ("ExternBlock", typeof<ExternBlock>);
    ("ToplevelBlock", typeof<ToplevelBlock>);
    ("CounterType", typeof<CounterType>);
    ("Type_Block", typeof<Type_Block>);
    ("Type_Counter", typeof<Type_Counter>);
    ("Type_Expression", typeof<Type_Expression>);
    ("Type_FieldListCalculation", typeof<Type_FieldListCalculation>);
    ("Type_Meter", typeof<Type_Meter>);
    ("Type_Register", typeof<Type_Register>);
    ("Type_AnyTable", typeof<Type_AnyTable>);
    ("HeaderOrMetadata", typeof<HeaderOrMetadata>);
    ("Header", typeof<Header>);
    ("HeaderStack", typeof<HeaderStack>);
    ("Metadata", typeof<Metadata>);
    ("HeaderRef", typeof<HeaderRef>);
    ("ConcreteHeaderRef", typeof<ConcreteHeaderRef>);
    ("HeaderStackItemRef", typeof<HeaderStackItemRef>);
    ("NamedRef", typeof<NamedRef>);
    ("If", typeof<If>);
    ("NamedCond", typeof<NamedCond>);
    ("Apply", typeof<Apply>);
    ("Primitive", typeof<Primitive>);
    ("FieldList", typeof<FieldList>);
    ("FieldListCalculation", typeof<FieldListCalculation>);
    ("CalculatedField_update_or_verify", typeof<CalculatedField_update_or_verify>);
    ("CalculatedField", typeof<CalculatedField>);
    ("CaseEntry", typeof<CaseEntry>);
    ("V1Parser", typeof<V1Parser>);
    ("ParserException", typeof<ParserException>);
    ("Attached", typeof<Attached>);
    ("Stateful", typeof<Stateful>);
    ("CounterOrMeter", typeof<CounterOrMeter>);
    ("Counter", typeof<Counter>);
    ("Register", typeof<Register>);
    ("PrimitiveAction", typeof<PrimitiveAction>);
    ("ActionArg", typeof<ActionArg>);
    ("ActionFunction", typeof<ActionFunction>);
    ("ActionProfile", typeof<ActionProfile>);
    ("ActionSelector", typeof<ActionSelector>);
    ("V1Table", typeof<V1Table>);
    ("V1Control", typeof<V1Control>);
    ("V1Program", typeof<V1Program>);
    ("v1HeaderType", typeof<v1HeaderType>);
    ("IntMod", typeof<IntMod>);
  |]
let TypeLookup = TypeNames |> Map.ofSeq
let Types = TypeNames |> Seq.map snd |> HashSet
  
open System.Text.RegularExpressions
let private regexMatches pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then
    // The first group is always the whole of the match, not one of the groups
    [for g in m.Groups -> g.Value] |> List.skip 1
  else []
let private (|Match|_|) pattern input =
  match regexMatches pattern input with
  | [] -> Option.None
  | l -> Option.Some l
let (*private*) splitTypeStrings (s : string) =
  let abCount (s:string) = s.ToCharArray() |> Seq.map (fun c -> match c with '<' -> 1 | '>' -> -1 | _ -> 0) |> Seq.sum
  seq {
    let mutable abc = 0
    let waiting = new System.Collections.Generic.List<string>()
    for part in s.Split(',') do
      waiting.Add part
      abc <- abc + abCount part
      if abc = 0 then
        yield String.concat "" waiting
        waiting.Clear()
  }
let rec GetTypeOf s : System.Type =
  printfn "GetTypeOf %s" s
  match s with
  | Match "^(?<Type>[^\<\>]*)\<(?<GenericParameters>.*)\>$" [t; p] ->
      let tType = GetTypeOf t
      let pTypes = splitTypeStrings p |> Seq.map GetTypeOf |> Seq.toArray
      tType.MakeGenericType pTypes
  | Match "^(?<Type>[^\<\>]*)$" [t] -> TypeLookup.[t]
  | _ -> failwith (sprintf "Invalid Node_Type %s" s)


[<Literal>]
let NodeId= "Node_ID" :> obj
[<Literal>]
let NodeType = "Node_Type" :> obj
[<Literal>]
let JType = "$type" :> obj
[<Literal>]
let JID = "$id" :> obj
[<Literal>]
let JRef = "$ref" :> obj

open Newtonsoft.Json.Linq
type IRConverter() =
  inherit JsonConverter()
  override this.CanConvert(objectType) =
    // Marker interfaces need to be converted because JSON.NET doesn't even know to read an object
    objectType = typeof<INode>
    || objectType = typeof<IDeclaration>
    || objectType = typeof<IContainer>
    || objectType = typeof<ICompileTimeValue>
  override this.CanWrite = false
  override this.WriteJson(writer, value, serialiser) = raise <| new System.InvalidOperationException("Use default serialization.")
  override this.ReadJson(reader, objectType, existingValue, serialiser) =
    if reader.TokenType <> JsonToken.StartObject then reader.Read() |> ignore
    let jo = JObject.Load(reader)
    let tname = jo.["$type"].Value<string>()
    let t = GetTypeOf tname
    jo.ToObject(t, serialiser)

open FSharpx.Collections
type AdvReader(reader, onRead) = 
  inherit JsonTextReader(reader) 
  let mutable queue = Deque.empty
  member private this.OnRead = onRead
  member private this.enqueue () = let hasToken = base.Read() in printfn "TOKEN %A %A" base.TokenType base.Value; queue <- queue.Conj ((base.TokenType, base.Value)); hasToken
  override this.TokenType = match queue.TryHead with Option.Some(t,_) -> t | Option.None -> base.TokenType 
  override this.Value = match queue.TryHead with Option.Some(_,v) -> v | Option.None -> base.Value 
  override this.Read() =
    if not queue.IsEmpty then
      queue <- queue.Tail
    let rec hasToken(readMore, queue') =
      printfn "hasToken (%A, %A)" readMore queue'
      queue <- queue'
      if (queue.IsEmpty || readMore) && this.enqueue() then
        hasToken (this.OnRead queue)
      else
        not queue.IsEmpty
    hasToken (false, queue)
  //override this.ReadAsBoolean() =

type saIRReader(reader) =
  inherit JsonTextReader(reader)
  override this.Read() =
    let hasToken = base.Read()
    if hasToken && base.TokenType = JsonToken.PropertyName then 
      match base.Value with
      | NodeType -> base.SetToken(JsonToken.PropertyName, JType)
      | NodeId -> base.SetToken(JsonToken.PropertyName, JID)
      | _ -> ()
    hasToken
type IRReader(reader) =
  inherit AdvReader(reader, IRReader.onRead)
  static member private onRead q =
    let readMore, ql =
      match Deque.toSeq q |> Seq.toList with
      | [(JsonToken.StartObject,_); (JsonToken.PropertyName,NodeId); (JsonToken.Integer,nid); (JsonToken.EndObject,_)] ->
          // This is a reference to another node - rewrite to $ref
          false, [(JsonToken.StartObject,null); (JsonToken.PropertyName,JRef); (JsonToken.String,nid); (JsonToken.EndObject,null)]
      | [(JsonToken.StartObject,_); (JsonToken.PropertyName,NodeId); (JsonToken.Integer,nid);
                                    (JsonToken.PropertyName,NodeType); (JsonToken.String,nty)] ->
          // This is not a reference, so just copy the id to $id and Node_Type to $type
          false, [(JsonToken.StartObject,null); (JsonToken.PropertyName,JType); (JsonToken.String,nty);
                                                (JsonToken.PropertyName,JID); (JsonToken.String,nid);
                                                (JsonToken.PropertyName,NodeId); (JsonToken.String,nid);
                                                (JsonToken.PropertyName,NodeType); (JsonToken.String,nty)]
      | ((JsonToken.StartObject,_)::ts) as ql ->
          match q.Last with
          | (JsonToken.StartObject, _) -> Deque.length q <> 1, ql
          | _ ->  Deque.length q < 5, ql // We want to read 5 tokens if the start is StartObject so we can check our match cases
      | ql -> false, ql
    readMore, Deque.ofList ql

type IRBinder() =
  inherit System.Runtime.Serialization.SerializationBinder()
  override this.BindToType(assemblyName, typeName) =
    printfn "Bind %s %s" assemblyName typeName
    GetTypeOf typeName

// FIXME for testing only
let testFile = "/working/part-ii-project/p4_16_samples/json/action_param.p4.json"

type IRReferenceResolver() =
  member private this.RefLookup : IDictionary<string,Node> = new Dictionary<string,Node>() :> IDictionary<string,Node>
  interface Newtonsoft.Json.Serialization.IReferenceResolver with
    member this.IsReferenced(context:obj, value:obj) =
      let node = value :?> Node
      this.RefLookup.ContainsKey (string node.Node_ID)
    member this.AddReference(context:obj, reference:string, value:obj) =
      printfn "AddReference %s" reference
      this.RefLookup.Add(reference, value :?> Node)
    member this.GetReference(context:obj, value:obj) =
      let node = value :?> Node
      string node.Node_ID
    member this.ResolveReference(context:obj, reference:string) =
      printfn "ResolveReference %s" reference
      let _, node = this.RefLookup.TryGetValue reference
      node :> obj

open System.IO
let deserialise filename =
  use reader = File.OpenText(filename)
  let serialiser = new JsonSerializer()
  serialiser.TypeNameHandling <- TypeNameHandling.Auto
  serialiser.MetadataPropertyHandling <- MetadataPropertyHandling.ReadAhead
  serialiser.Binder <- new IRBinder()
  serialiser.PreserveReferencesHandling <- PreserveReferencesHandling.Objects
  serialiser.ReferenceResolver <- new IRReferenceResolver()
  serialiser.Converters.Add(new OrderedMapConverter())
  serialiser.Converters.Add(new IRConverter())
  serialiser.Converters.Add(new DirectionJsonConverter())
  serialiser.Deserialize<P4Program>(new IRReader(reader))