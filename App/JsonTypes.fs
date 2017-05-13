(*
  Copyright 2016 Jonny Shipton

  This file contains the IR structures for code parsed by p4c and dumped as JSON.
*)

namespace P4ToCSharp.App.IR

open Newtonsoft.Json // For attributes

open P4ToCSharp.App.Util

// FIXME remove types which aren't actually generated. E.g. are V1 types ever generated, or are they always converted?

module JsonTypes =
  type IDictionary<'K,'V> = System.Collections.Generic.IDictionary<'K,'V>
  type Dictionary<'K,'V> = System.Collections.Generic.Dictionary<'K,'V>
  type ILookup<'K,'V> = System.Linq.ILookup<'K,'V>
  type HashSet<'T> = System.Collections.Generic.HashSet<'T>

  // Original IR uses a struct with original name and source info, but only name is serialised
  type ID = string

  // Helpful interfaces
  type INamed =
    abstract member Name : ID

  // Marker interfaces
  // FIXME it's really annoying how these don't guarantee they are Nodes
  type INode = interface end
  type IDeclaration = inherit INode
  type IContainer = inherit IDeclaration
  type ICompileTimeValue = inherit INode

  // FIXME Makeshift map types + should be immutable
  type OrderedMap<'K, 'V> = ('K * 'V) array // NOTE these makeshift map types are dependent on by OrderedMapConverter
  type OrderedMultiMap<'K, 'V> = ('K * 'V list) array
  type MultiMap<'K, 'V> = ILookup<'K, 'V>
  type UnorderedMap<'K, 'V> = IDictionary<'K, 'V>
  type vector<'T> = 'T array // FIXME having a difference only in case between vector and Vector is confusing


  //-----------------------------------------------------------------------------------------------
  // Now follows the types which will be deserialised from the JSON output of the p4c compiler IR.
  //-----------------------------------------------------------------------------------------------
  let mutable private maxNodeId = 0
  type Node [<JsonConstructor>](node_id, node_type) =
    inherit obj()
    interface INode
    member val Node_ID : int =
      let node_id = if node_id < 0 then maxNodeId else node_id // Negative node_id not allowed - give new id instead
      if node_id >= maxNodeId then maxNodeId <- node_id + 1
      node_id
    member this.Node_Type : string = node_type
    // Gets any elements which could be accessed by an identifer in a parent scope. (External access)
    abstract member NamedChild : string -> Node option
    default this.NamedChild(name) = None
    // Gets any elements which could be accessed by an identifier in this or a child scope. (Internal access)
    abstract member NamedInScope : string -> Node option
    default this.NamedInScope(name) = None
    override this.Equals(other) =
      match other with
      | :? Node as other -> this.Node_ID >= 0 && this.Node_ID = other.Node_ID
      | _ -> base.Equals(other)

  type Vector<'T> [<JsonConstructor>](node_id, node_type, vec) =
    inherit Node(node_id, node_type)
    new(vec) = Vector(-1, "Vector", vec)
    member this.vec : vector<'T> = vec // value is json list

  [<Sealed>]
  type IndexedVector<'T> [<JsonConstructor>](node_id, node_type, vec, declarations) =
    inherit Vector<'T>(node_id, node_type, vec)
    new(declarations) = IndexedVector(-1, "IndexedVector", declarations |> Array.map snd, declarations |> Array.map (fun (k,v) -> k, v :> obj :?> IDeclaration))
    member this.declarations : OrderedMap<string, IDeclaration> = declarations // value is json dictionary
    member this.declarationsMap = declarations |> Map.ofSeq

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
  type Type_Unknown [<JsonConstructor>](node_id, node_type) =
    inherit Type_Base(node_id, node_type)
    new() = Type_Unknown(-1, "Type_Unknown")

  type StatOrDecl(node_id, node_type) =
    inherit Node(node_id, node_type)

  type Declaration(node_id, node_type, name, declid) =
    inherit StatOrDecl(node_id, node_type)
    interface IDeclaration
    member this.name : ID = name
    member this.declid : int = declid
    interface INamed with
      member this.Name = this.name

  [<AbstractClass>]
  type Type_Declaration(node_id, node_type, name, declid) =
    inherit Type(node_id, node_type)
    interface IDeclaration
    member this.name : ID = name
    member this.declid : int = declid
    interface INamed with
      member this.Name = this.name

  [<AbstractClass>]
  type Expression(node_id, node_type, type_) =
    inherit Node(node_id, node_type)
    [<JsonPropertyAttribute("type")>]
    member this.type_ : Type = type_

  [<AbstractClass>]
  type Operation(node_id, node_type, type_) =
    inherit Expression(node_id, node_type, type_)

  [<Sealed>]
  type Path [<JsonConstructor>](node_id, node_type, name, absolute) =
    inherit Node(node_id, node_type)
    new(name, ?absolute) =
      let absolute = defaultArg absolute false
      Path(-1, "Path", name, absolute)
    member this.name : ID = name
    member this.absolute : bool = absolute
    override this.ToString() =
      (if this.absolute then "." else "") + this.name
    override this.Equals(other) =
      match other with
      | :? Path as other ->
        (this.Node_ID >= 0 && this.Node_ID = other.Node_ID)
        || (this.absolute = other.absolute && this.name = other.name)
      | _ -> false

  [<Sealed>]
  type Annotation(node_id, node_type, name, expr) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.expr : Vector<Expression> = expr
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type Annotations(node_id, node_type, annotations) =
    inherit Node(node_id, node_type)
    member this.annotations : Vector<Annotation> = annotations
    member this.GetAnnotationByName<'a>(name) =
      this.annotations.vec
      |> Seq.filter (fun annotation -> annotation.name = name)
      |> Seq.cast<'a>
      |> Seq.tryFirst

  type Direction = NoDirection | In | Out | InOut (*  NOTE P4 has copy-in/copy-out semantics *)

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
    override this.NamedChild(name) =
      match this.fields.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.fields.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

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
    override this.NamedInScope(name) =
      match this.typeParameters.parameters.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Package(node_id, node_type, name, declid, annotations, typeParameters, constructorParams) =
    inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
    interface IContainer
    member this.constructorParams : ParameterList = constructorParams
    override this.NamedInScope(name) =
      match this.constructorParams.parameters.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Parser(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
    inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
    member this.applyParams : ParameterList = applyParams
    override this.NamedInScope(name) =
      match this.applyParams.parameters.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Control(node_id, node_type, name, declid, annotations, typeParameters, applyParams) =
    inherit Type_ArchBlock(node_id, node_type, name, declid, annotations, typeParameters)
    member this.applyParams : ParameterList = applyParams
    override this.NamedInScope(name) =
      match this.applyParams.parameters.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Name(node_id, node_type, path) =
    inherit Type(node_id, node_type)
    member this.path : Path = path
    override this.ToString() =
      this.path.ToString()
    override this.Equals(other) =
      match other with
      | :? Type_Name as other ->
        (this.Node_ID >= 0 && this.Node_ID = other.Node_ID)
        || this.path = other.path
      | _ -> false

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
    override this.NamedChild(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

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
    member this.GetPropertyByName(name) =
      this.properties.vec
      |> Seq.filter (fun prop -> prop.name = name)
      |> Seq.tryFirst
    member this.GetPropertyValueByName<'a>(name) =
      this.GetPropertyByName(name)
      |> Option.map (fun p -> p.value)
      |> Option.cast<_,'a>

  [<Sealed>]
  type P4Table(node_id, node_type, name, declid, annotations, parameters, properties) =
    inherit Declaration(node_id, node_type, name, declid)
    member this.annotations : Annotations = annotations
    member this.parameters : ParameterList = parameters
    member this.properties : TableProperties = properties
    override this.NamedChild(name) =
      this.properties.properties.declarationsMap.TryFind name |> Option.cast
    override this.NamedInScope(name) =
      match this.parameters.parameters.declarationsMap.TryFind name
            |> Option.tryIfNone (fun () -> this.properties.properties.declarationsMap.TryFind name) |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Table(node_id, node_type, table) =
    inherit Type(node_id, node_type)
    member this.table : P4Table = table
    override this.NamedChild(name) =
      table.NamedChild(name)

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
    override this.NamedChild(name) = // FIXME is this correct?
      match this.actionList.actionList.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x

  [<AbstractClass>]
  type Type_MethodBase(node_id, node_type, typeParameters, returnType, parameters) =
    inherit Type(node_id, node_type)
    member this.typeParameters : TypeParameters = typeParameters
    member this.returnType : Type option = returnType // if != nullptr
    member this.parameters : ParameterList = parameters
    override this.NamedInScope(name) =
      match this.parameters.parameters.declarationsMap.TryFind name
            |> Option.tryIfNone (fun () -> this.typeParameters.parameters.declarationsMap.TryFind name)|> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

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
    override this.NamedChild(name) =
      match this.type_.NamedChild(name) with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.type_.NamedInScope(name) with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Typedef(node_id, node_type, name, declid, annotations, type_) =
    inherit Type_Declaration(node_id, node_type, name, declid)
    member this.annotations : Annotations = annotations
    [<JsonProperty("type")>]
    member this.type_ : Type = type_ // type
    // FIXME does this need NamedChild override, or are we manually following type?

  [<Sealed>]
  type NameList(node_id, node_type, names) =
    inherit Node(node_id, node_type)
    member this.names : vector<ID> = names

  [<Sealed>]
  type Attribute(node_id, node_type, name, declid, type_, locals, optional) =
    inherit Declaration(node_id, node_type, name, declid)
    [<JsonProperty("type")>]
    member this.type_ : Type option = type_ // type; if != nullptr
    member this.locals : NameList option = locals // if != nullptr
    member this.optional : bool = optional

  [<Sealed>]
  type Type_Extern(node_id, node_type, name, declid, typeParameters, methods, attributes, annotations) =
    inherit Type_Declaration(node_id, node_type, name, declid)
    member this.typeParameters : TypeParameters = typeParameters
    member this.methods : Vector<Method> = methods
    member this.attributes : NameMap<Attribute> = attributes // ordered_map
    member this.annotations : Annotations = annotations
    override this.NamedChild(name) =
      match this.methods.vec |> Seq.filter (fun meth -> meth.name = name) |> Seq.tryFirst |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.methods.vec |> Seq.filter (fun meth -> meth.name = name) |> Seq.tryFirst |> Option.cast
            |> Option.tryIfNone (fun () -> this.typeParameters.parameters.declarationsMap.TryFind name |> Option.cast) with
      | None -> base.NamedInScope name
      | x -> x

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
    member this.value : int = value // FIXME can also be bigint
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
  type PathExpression [<JsonConstructor>](node_id, node_type, type_, path) =
    inherit Expression(node_id, node_type, type_)
    new(path, ?type_) =
      let type_ = defaultArg type_ (Type_Unknown())
      PathExpression(-1, "PathExpression", type_, path)
    member this.path : Path = path
    override this.ToString() =
      this.path.ToString()

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
    override this.ToString() =
      sprintf "%O.%s" this.expr this.member_

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
    // NOTE rely on thisMap, cannot implement NamedChild

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
  type MethodCallExpression [<JsonConstructor>](node_id, node_type, type_, method_, typeArguments, arguments) =
    inherit Expression(node_id, node_type, type_)
    new(method_, arguments, ?typeArguments, ?type_) =
      let typeArguments = defaultArg typeArguments (Vector([||]))
      let type_ = defaultArg type_ (Type_Unknown())
      MethodCallExpression(-1, "MethodCallExpression", type_, method_, typeArguments, arguments)
    [<JsonProperty("method")>]
    member this.method_ : Expression = method_ // method
    member this.typeArguments : Vector<Type> = typeArguments
    member this.arguments : Vector<Expression> = arguments
    member this.WithTypeArguments(types : Type seq) =
      new MethodCallExpression(node_id, node_type, type_, method_, Vector(types |> Seq.toArray), arguments)
    member this.WithArguments(args : Expression seq) =
      new MethodCallExpression(node_id, node_type, type_, method_, typeArguments, Vector(args |> Seq.toArray))

  [<Sealed>]
  type ConstructorCallExpression(node_id, node_type, type_, arguments) =
    inherit Expression(node_id, node_type, type_)
    member this.constructedType : Type = this.type_
    member this.arguments : Vector<Expression> = arguments

  [<Sealed>]
  type ParserState(node_id, node_type, name, declid, annotations, components, selectExpression) =
    inherit Declaration(node_id, node_type, name, declid)
    member this.annotations : Annotations = annotations
    member this.components : IndexedVector<StatOrDecl> = components // NOTE parser internals are not publically visible?
    member this.selectExpression : Expression option = selectExpression // if != nullptr
    override this.NamedInScope(name) =
      match this.components.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type P4Parser(node_id, node_type, name, declid, type_, constructorParams, parserLocals, states) =
    inherit Type_Declaration(node_id, node_type, name, declid)
    interface IContainer
    [<JsonProperty("type")>]
    member this.type_ : Type_Parser = type_ // type
    member this.constructorParams : ParameterList = constructorParams
    member this.parserLocals : IndexedVector<Declaration> = parserLocals // NOTE parser internals are not publically visible?
    member this.states : IndexedVector<ParserState> = states
    // FIXME do we need to expose apply as a NamedChild? No, because there is nothing to return? apply is a synthesised method. Treat it as a special case?
    override this.NamedInScope(name) =
      match this.constructorParams.parameters.declarationsMap.TryFind name
            |> Option.tryIfNone (fun () -> this.parserLocals.declarationsMap.TryFind name)
            |> Option.tryIfNone (fun () -> this.states.declarationsMap.TryFind name) |> Option.cast
            |> Option.tryIfNone (fun () -> this.type_.NamedInScope name) with
      | None -> base.NamedInScope name
      | x -> x

  [<AbstractClass>]
  type Statement(node_id, node_type) =
    inherit StatOrDecl(node_id, node_type)

  [<Sealed>]
  type BlockStatement(node_id, node_type, annotations, components) =
    inherit Statement(node_id, node_type)
    member this.annotations : Annotations = annotations
    member this.components : IndexedVector<StatOrDecl> = components
    override this.NamedInScope(name) =
      // FIXME we are ignoring shadowing?
      match this.components.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type P4Control(node_id, node_type, name, declid, type_, constructorParams, controlLocals, body) =
    inherit Type_Declaration(node_id, node_type, name, declid)
    interface IContainer
    [<JsonProperty("type")>]
    member this.type_ : Type_Control = type_ // type
    member this.constructorParams : ParameterList = constructorParams
    member this.controlLocals : IndexedVector<Declaration> = controlLocals
    member this.body : BlockStatement = body
    // FIXME do we need to expose apply as a NamedChild? No, because there is nothing to return? apply is a synthesised method. Treat it as a special case?
    override this.NamedInScope(name) =
      match this.constructorParams.parameters.declarationsMap.TryFind name
            |> Option.tryIfNone (fun () -> this.controlLocals.declarationsMap.TryFind name) |> Option.cast
            |> Option.tryIfNone (fun () -> this.type_.NamedInScope name) with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type P4Action(node_id, node_type, name, declid, annotations, parameters, body) =
    inherit Declaration(node_id, node_type, name, declid)
    member this.annotations : Annotations = annotations
    member this.parameters : ParameterList = parameters
    member this.body : BlockStatement = body
    override this.NamedInScope(name) =
      match this.parameters.parameters.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Type_Error(node_id, node_type, name, declid, members) =
    inherit Type_Declaration(node_id, node_type, name, declid)
    member this.members : IndexedVector<Declaration_ID> = members
    override this.NamedChild(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type Declaration_MatchKind [<JsonConstructor>](node_id, node_type, members) =
    inherit Node(node_id, node_type)
    new(members) = Declaration_MatchKind(-1, "Declaration_MatchKind", members)
    member this.members : IndexedVector<Declaration_ID> = members
    override this.NamedChild(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.members.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

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
    member this.initializer : Expression option = initializer // if != nullptr

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
    member this.initializer : BlockStatement option = initializer // if != nullptr
    // FIXME NamedChild/NamedInScope?

  [<Sealed>]
  type P4Program(node_id, node_type, declarations) =
    inherit Node(node_id, node_type)
    member this.declarations : IndexedVector<Node> = declarations
    override this.NamedChild(name) =
      match this.declarations.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedChild name
      | x -> x
    override this.NamedInScope(name) =
      match this.declarations.declarationsMap.TryFind name |> Option.cast with
      | None -> base.NamedInScope name
      | x -> x

  [<Sealed>]
  type ExitStatement(node_id, node_type) =
    inherit Statement(node_id, node_type)

  [<Sealed>]
  type ReturnStatement(node_id, node_type, expression) =
    inherit Statement(node_id, node_type)
    member this.expression : Expression option = expression // if != nullptr

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
    member this.ifFalse : Statement option = ifFalse // if != nullptr

  [<Sealed>]
  type MethodCallStatement(node_id, node_type, methodCall) =
    inherit Statement(node_id, node_type)
    member this.methodCall : MethodCallExpression = methodCall

  [<Sealed>]
  type SwitchCase(node_id, node_type, label, statement) =
    inherit Node(node_id, node_type)
    member this.label : Expression = label
    member this.statement : Statement option = statement // if != nullptr

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
    override this.NamedInScope(name) =
      match this.type_.NamedInScope name with
      | None -> base.NamedInScope name
      | x -> x

  // FIXME not sure about NamedChild/NamedInScope for any of these (Block/children)
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

  // FIXME not sure about NamedChild/NamedInScope for these (HeaderOrMetadata/children)
  [<AbstractClass>]
  type HeaderOrMetadata(node_id, node_type, type_name, name, annotations, type_) =
    inherit Node(node_id, node_type)
    member this.type_name : ID = type_name
    member this.name : ID = name
    member this.annotations : Annotations = annotations
    [<JsonProperty("type")>]
    member this.type_ : Type_StructLike option = type_ // type; if != nullptr
    interface INamed with
      member this.Name = this.name

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
    interface INamed with
      member this.Name = this.name

  type If(node_id, node_type, type_, pred, ifTrue, ifFalse) =
    inherit Expression(node_id, node_type, type_)
    member this.pred : Expression = pred
    member this.ifTrue : Vector<Expression> option = ifTrue // if != nullptr
    member this.ifFalse : Vector<Expression> option = ifFalse // if != nullptr

  [<Sealed>]
  type NamedCond(node_id, node_type, type_, pred, ifTrue, ifFalse, name) =
    inherit If(node_id, node_type, type_, pred, ifTrue, ifFalse)
    member this.name : string = name
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type Apply(node_id, node_type, type_, name, actions) =
    inherit Expression(node_id, node_type, type_)
    member this.name : ID = name
    member this.actions : NameMap<Vector<Expression>> = actions
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type Primitive(node_id, node_type, type_, name, operands) =
    inherit Operation(node_id, node_type, type_)
    member this.name : string = name
    member this.operands : Vector<Expression> = operands
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type FieldList(node_id, node_type, name, payload, annotations, fields) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.payload : bool = payload
    member this.annotations : Annotations = annotations
    member this.fields : Vector<Expression> = fields
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type FieldListCalculation(node_id, node_type, name, input, algorithm, output_width, annotations) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.input : NameList option = input // if != nullptr
    member this.algorithm : ID = algorithm
    member this.output_width : int = output_width
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

  type CalculatedField_update_or_verify =
    { update : bool;
      name : ID;
      cond : Expression; }
  [<Sealed>]
  type CalculatedField(node_id, node_type, field, specs, annotations) =
    inherit Node(node_id, node_type)
    member this.field : Expression option = field // if != nullptr
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
    member this.stmts : Vector<Expression> = stmts  // NOTE parser internals are not publically visible?
    member this.select : Vector<Expression> option = select // if != nullptr
    member this.cases : Vector<CaseEntry> option = cases // if != nullptr
    member this.default_return : ID = default_return
    member this.parse_error : ID = parse_error
    member this.drop : bool = drop
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type ParserException(node_id, node_type) =
    inherit Node(node_id, node_type)

  [<AbstractClass>]
  type Attached(node_id, node_type, name, annotations) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

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
    member this.result : Expression option = result // if != nullptr
    member this.pre_color : Expression option = pre_color // if != nullptr
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
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type ActionFunction(node_id, node_type, name, action, args, annotations) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.action : Vector<Primitive> = action
    member this.args : vector<ActionArg> = args
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

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
    member this.reads : Vector<Expression> option = reads // if != nullptr
    member this.reads_types : vector<ID> = reads_types
    member this.min_size : int = min_size
    member this.max_size : int = max_size
    member this.size : int = size
    member this.action_profile : ID = action_profile
    member this.actions : vector<ID> = actions
    member this.default_action : ID = default_action
    member this.default_action_args : Vector<Expression> option = default_action_args // if != nullptr
    member this.properties : TableProperties = properties
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type V1Control(node_id, node_type, name, code, annotations) =
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.code : Vector<Expression> = code
    member this.annotations : Annotations = annotations
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type V1Program(node_id, node_type, scope) =
    inherit Node(node_id, node_type)
    member this.scope : NameMap<Node> = scope // multimap

  [<Sealed>]
  type v1HeaderType(node_id, node_type, name, as_metadata, as_header) = // sic
    inherit Node(node_id, node_type)
    member this.name : ID = name
    member this.as_metadata : Type_Struct = as_metadata
    member this.as_header : Type_Header option = as_header // if != nullptr
    interface INamed with
      member this.Name = this.name

  [<Sealed>]
  type IntMod(node_id, node_type, type_, expr, width) =
    inherit Operation_Unary(node_id, node_type, type_, expr)
    member this.width : uint32 = width


  let TypeLookup, Types =
    let TypeNames = // FIXME Could this be constructed by reflection of all types : Node in this module + the others?
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
    (TypeNames |> Map.ofSeq, TypeNames |> Seq.map snd |> HashSet)

  type KeyValuePair<'key,'value> =
    {
      Key : 'key;
      Value : 'value;
    }

  type TypeMap =
    {
      Map : KeyValuePair<int,Type> seq
    }

  type RefMap =
    {
      PathToDeclaration : KeyValuePair<int,IDeclaration> seq;
      ThisToDeclaration : KeyValuePair<int,IDeclaration> seq
    }

  type Program =
    {
      P4 : P4Program;
      TypeMap : Map<int, Type>;
      PathMap : Map<int, IDeclaration>;
      ThisMap : Map<int, IDeclaration>;
    }

  open P4ToCSharp.Library
  let P4TypeOf (node:Node) =
    match node with
    | :? P4Action -> P4Type.Action
    | :? Declaration_Constant -> P4Type.Const
    | :? Type_Control | :? P4Control -> P4Type.Control
    | :? Type_Enum -> P4Type.Enum
    | :? Type_Error -> P4Type.Error
    | :? Method -> P4Type.ExternFunction
    | :? Type_Extern -> P4Type.ExternObject
    | :? Type_Header -> P4Type.Header
    | :? Declaration_MatchKind -> P4Type.MatchKind
    | :? Type_Package | :? Declaration_Instance -> P4Type.Package // NOTE Declaration_Instance could also be an extern local, but this function isn't used in that case (I think)
    | :? Type_Parser | :? P4Parser -> P4Type.Parser
    | :? Type_Struct -> P4Type.Struct
    | _ -> failwithf "JsonTypes.%s does not have a corresponding P4Type" (node.GetType().Name)
    // FIXME Check these are the correct JsonTypes types
    // FIXME What about instantiation?
    // NOTE typedef is valid, but has no P4Type
    // FIXME What about union? (Not in spec yet?)