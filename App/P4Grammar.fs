namespace P4ToCSharp.App

module P4Grammar =
  
  type P4Program = Declaration list

  and Declaration =
    | Constant of ConstantDeclaration
    | ExternType of ExternTypeDeclaration
    | ExternFunction of ExternFunctionDeclaration
    | Action of ActionDeclaration
    | Parser of ParserDeclaration
    | Type of TypeDeclaration
    | Control of ControlDeclaration
    | Instantiation of Instantiation
    | Error of ErrorDeclaration
    | MatchKind of MatchKindDeclaration

  and Annotation =
    Annotation of
        name:string
      * expr:Expression

  and Parameter =
    Parameter of
        direction:Direction
      * ty:Type * name:string

  and [<RequireQualifiedAccess>]
    Direction = In | Out | InOut | None

  and PackageTypeDeclaration =
    PackageTypeDeclaration of
        annotations:Annotation list
      * name:string
      * tyParams:TypeVar list
      * parameters:Parameter list

  and Instantiation =
    Instantiation of
        annotations:Annotation list
      * ty:Type
      * arguments:Expression list
      * name:string

  and ParserDeclaration =
    ParserDeclaration of
        parserType:ParserTypeDeclaration
      * ctorParams:Parameter list
      * parserLocalElements:ParserLocalElement list
      * parserStates:ParserState list

  and [<RequireQualifiedAccess>]
    ParserLocalElement =
    | ConstantDeclaration of ConstantDeclaration
    | VariableDeclaration of VariableDeclaration
    | Instantiation of Instantiation

  and ParserTypeDeclaration =
    ParserTypeDeclaration of
        annotations:Annotation list
      * name:string
      * typeParameters:TypeVar list
      * parameters:Parameter list

  and ParserState =
    ParserState of
        annotations:Annotation list
      * name:string
      * parserStatements:ParserStatement list
      * transitionStatement:StateExpression

  and [<RequireQualifiedAccess>]
    ParserStatement =
    | Assignment of AssignmentStatement
    | MethodCall of MethodCall
    | DirectApplication of DirectApplication
    | ParserBlockStatement of annotations:Annotation list * parserStatements:ParserStatement list
    | ConstantDeclaration of ConstantDeclaration
    | VariableDeclaration of VariableDeclaration

  and [<RequireQualifiedAccess>]
    StateExpression =
    | Name of string
    | Select of SelectExpression

  and SelectExpression =
    SelectExpression of
        expr:Expression list
      * selectCases:SelectCase list

  and SelectCase =
    SelectCase of
        keysetExpr:KeysetExpression
      * name:string

  and [<RequireQualifiedAccess>]
    KeysetExpression =
    | TupleKeysetExpression of SimpleKeysetExpression * SimpleKeysetExpression
    | SimpleKeysetExpression of SimpleKeysetExpression

  and [<RequireQualifiedAccess>]
    SimpleKeysetExpression =
    | Expression of Expression
    | Default
    | Don'tCare
    | Mask of Expression * Expression
    | Range of Expression * Expression

  and ControlDeclaration =
    ControlDeclaration of
        controlType:ControlTypeDeclaration
      * ctorParameters:Parameter list
      * controlLocalDeclarations:ControlLocalDeclaration list
      * controlBody:BlockStatement

  and ControlTypeDeclaration =
    ControlTypeDeclaration of
        annotations:Annotation list
      * name:string
      * tyParameters:TypeVar list
      * parameters:Parameter list

  and [<RequireQualifiedAccess>]
    ControlLocalDeclaration =
    | ConstantDeclaration of ConstantDeclaration
    | ActionDeclaration of ActionDeclaration
    | TableDeclaration of TableDeclaration
    | Instantiation of Instantiation
    | VariableDeclaration of VariableDeclaration

  and ExternTypeDeclaration =
    ExternTypeDeclaration of
        annotations:Annotation list
      * name:string
      * tyParameters:TypeVar list
      * methods:MethodPrototype list

  and ExternFunctionDeclaration =
    ExternFunctionDeclaration of
        annotations:Annotation list
      * functionPrototype:FunctionPrototype

  and FunctionPrototype =
    FunctionPrototype of
        returnType:ReturnType
      * tyParameters:TypeVar list
      * parameters:Parameter list

  and [<RequireQualifiedAccess>]
    MethodPrototype =
    | FunctionPrototype of FunctionPrototype
    | ConstructorPrototype of parameters:Parameter list
    
  and [<RequireQualifiedAccess>]
    Type =
    | Bool
    | Error
    | Bit of width:uint32
    | Int of width:uint32
    | VarBit of maxWidth:uint32
    | TypeName of PrefixedName
    | SpecialisedType of ty:PrefixedName * tyArguments:TypeArgument list
    | HeaderStackType of ty:PrefixedName * size:Expression
    | TupleType of TypeArgument list

  and PrefixedName =
    PrefixedName of
        qualification:GlobalQualification
      * name:string

  and [<RequireQualifiedAccess>]
    GlobalQualification =
    | Global
    | NonGlobal

  and [<RequireQualifiedAccess>]
    ReturnType =
    | Type of Type
    | Void
    | TypeVar of TypeVar

  and [<RequireQualifiedAccess>]
    TypeArgument =
    | Don'tCare
    | Type of Type

  and TypeVar = TypeVar of name:string

  and [<RequireQualifiedAccess>]
    TypeDeclaration =
    | Derived of DerivedTypeDeclaration
    | TypeDef of TypeDefDeclaration
    | Parser of ParserTypeDeclaration
    | Control of ControlTypeDeclaration
    | Package of PackageTypeDeclaration

  and [<RequireQualifiedAccess>]
    DerivedTypeDeclaration =
    | HeaderTypeDeclaration of annotations:Annotation list * name:string * fields:StructField list
    | HeaderUnionDeclaration of annotations:Annotation list * name:string * fields:StructField list
    | StructTypeDeclaration of annotations:Annotation list * name:string * fields:StructField list
    | EnumDeclaration of annotations:Annotation list * name:string * identifiers:string list

  and StructField = StructField of annotations:Annotation list * ty:Type * name:string

  and ErrorDeclaration = ErrorDeclaration of identifiers:string list

  and MatchKindDeclaration = MatchKindDeclaration of identifiers:string list

  and [<RequireQualifiedAccess>]
    TypeDefDeclaration =
    | Simple of annotations:Annotation list * ty:Type * name:string
    | Derived of annotations:Annotation list * ty:DerivedTypeDeclaration * name:string

  and AssignmentStatement =
    AssignmentStatement of
        lvalue:LValue
      * expr:Expression

  and MethodCall =
    MethodCall of
        methd:LValue
      * tyArguments:TypeArgument list
      * arguments:Expression list

  and DirectApplication =
    DirectApplication of
        tyName:PrefixedName
      * arguments:Expression list

  and [<RequireQualifiedAccess>]
    Statement =
    | Assignment of AssignmentStatement
    | MethodCall of MethodCall
    | DirectApplication of DirectApplication
    | Conditional of condition:Expression * thn:Statement * els:Statement option
    | Empty
    | Block of BlockStatement
    | Exit
    | Return
    | Switch of matchExpr:Expression * cases:SwitchCase list

  and BlockStatement =
    BlockStatement of
        annotations:Annotation list
      * StatementOrDeclaration list

  and SwitchCase = SwitchCase of label:SwitchLabel * block:BlockStatement option

  and [<RequireQualifiedAccess>]
    SwitchLabel =
    | Label of string
    | Default

  and [<RequireQualifiedAccess>]
    StatementOrDeclaration =
    | Variable of VariableDeclaration
    | Constant of ConstantDeclaration
    | Statement of Statement
    | Instantiation of Instantiation

  and TableDeclaration =
    TableDeclaration of
        annotations:Annotation list
      * name:string
      * properties:TableProperty list

  and [<RequireQualifiedAccess>]
    TableProperty =
    | Key of elements:KeyElement list
    | Actions of actions:ActionRef list
    | ConstEntries of Entry list
    | Property of isConst:IsConst * identifier:string * initialiser:Expression

  and [<RequireQualifiedAccess>]
    IsConst = Const | NotConst

  and KeyElement =
    KeyElement of
        expr:Expression
      * name:string
      * annotations:Annotation list

  and Entry =
    Entry of
        annotations:Annotation list
      * keysetExpr:KeysetExpression
      * action:ActionRef

  and ActionRef =
    ActionRef of
        annotations:Annotation list
      * name:string
      * arguments:Expression list

  and ActionDeclaration =
    ActionDeclaration of
        annotations:Annotation list
      * name:string
      * parameters:Parameter list
      * body:BlockStatement

  and VariableDeclaration =
    VariableDeclaration of
        annotations:Annotation list
      * ty:Type
      * name:string
      * initialiser:Expression option

  and ConstantDeclaration =
    ConstantDeclaration of
        annotations:Annotation list
      * ty:Type
      * name:string
      * initaliser:Expression

  and [<RequireQualifiedAccess>]
    LValue =
    | Name of PrefixedName
    | MemberAccess of LValue * string
    | Index of LValue * Expression
    | RangeIndex of LValue * Expression * Expression

  and [<RequireQualifiedAccess>]
    Expression =
    | Int of bigint
    | SizedInt of width:int * value:bigint
    | Bool of bool
    | String of string
    | Name of PrefixedName
    | Index of LValue * Expression
    | RangeIndex of LValue * Expression * Expression
    | Tuple of Expression list
    | Paren of Expression
    | UnaryOp of UnaryOp * Expression
    | MemberAccess of Expression * string
    | BinaryOp of BinaryOp * Expression * Expression
    | MethodCall of MethodCall
    | TypeConstruction of Type * Expression list
    | Cast of Type * Expression
    
  and [<RequireQualifiedAccess>]
    UnaryOp =
    | LNot | BNot | Neg | Pos

  and [<RequireQualifiedAccess>]
    BinaryOp =
    | Mul | Div | Rem | Add | Sub
    | Shl | Shr
    | Leq | Geq | Lt | Gt | NEq | Eq
    | BAnd | BOr | BXOr | Concat
    | LAnd | LOr

