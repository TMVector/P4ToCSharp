module P4ToCSharp.App.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
type SF = SyntaxFactory
type SK = SyntaxKind
type Expr = Syntax.ExpressionSyntax

open System
open System.Reflection

open P4ToCSharp.App.Common
open P4ToCSharp.App.Util
open P4ToCSharp.App.IR
open P4ToCSharp.App.CSharpTypes

// Short names for attribute types
type p4Type = P4ToCSharp.Library.P4Type
type p4Attribute = P4ToCSharp.Library.P4Attribute
type p4LookupAttribute = P4ToCSharp.Library.P4LookupAttribute
type p4ArchitectureAttribute = P4ToCSharp.Library.P4ArchitectureAttribute
let p4AttributeType = typeof<p4Attribute>
let p4LookupAttributeType = typeof<p4LookupAttribute>
let p4ArchitectureAttributeType = typeof<p4ArchitectureAttribute>
let libraryErrorType = typeof<P4ToCSharp.Library.error>

type AnnotationVerdict =
  | Gen // Generated ONLY for the purpose of the interface
  | Model // Actually used as implementation
  | Invalid // Not valid
type TypeOrMember = Type of Type | Member of MemberInfo
  with
  member this.CategoryString = match this with Type _ -> "type" | Member _ -> "member"
type ArchClassRequirement = RequireArchClass | DontRequireArchClass
let private validCsForP4 (p4:p4Attribute) (cs:TypeOrMember) : (AnnotationVerdict * compilerError list) = // NOTE this doesn't check signatures etc.; maybe rename to avoid confustion
  match p4.Type, cs with
  // Gen items (we use a when clause so control will continue to Model checks if this isn't Gen)
  | (p4Type.ExternObject | p4Type.Package), Type ty when ty.IsInterface -> (Gen, [])
  // Static methods
  | (p4Type.Action | p4Type.ExternFunction), Member m ->
      if not (m.MemberType = MemberTypes.Method) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a method." p4.P4Path p4.Type ])
      else (Model, [])
  // Static readonly or const fields/properties
  | (p4Type.Const), Member m ->
      if m.MemberType = MemberTypes.Field then
        let field = m :?> FieldInfo
        if not (field.IsLiteral || field.IsInitOnly) then (Invalid, [ warningf "A field annotated with P4Attribute(\"%s\", %A) must be const or static readonly." p4.P4Path p4.Type ])
        else (Model, [])
      else if m.MemberType = MemberTypes.Property then
        let prop = m :?> PropertyInfo
        if not (prop.CanRead && not prop.CanWrite) then (Invalid, [ warningf "A property annotated with P4Attribute(\"%s\", %A) must be read-only." p4.P4Path p4.Type ])
        else (Model, [])
      else (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a field or a property." p4.P4Path p4.Type ])
  // Interfaces
  | (p4Type.Control | p4Type.Parser), Type ty ->
      if not (ty.IsInterface) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate an interface." p4.P4Path p4.Type ])
      else (Model, [])
  // Enums
  | (p4Type.Enum | p4Type.MatchKind), Type ty ->
      if not (ty.IsEnum) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate an enum." p4.P4Path p4.Type ])
      else (Model, [])
  // Classes
  | (p4Type.ExternObject), Type ty ->
      if not (ty.IsClass) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a class or struct." p4.P4Path p4.Type ])
      else (Model, [])
  // Non-static classes
  | (p4Type.Error | p4Type.Header | p4Type.Package | p4Type.Struct), Type ty ->
      if not (ty.IsClass && not (ty.IsAbstract && ty.IsSealed)) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a non-static class." p4.P4Path p4.Type ])
      else (Model, [])
  // Should be Type/Member, but is Member/Type
  | _ -> (Invalid, [ warningf "P4Attribute(\"%s\", %A) cannot annotate a %s." p4.P4Path p4.Type cs.CategoryString ])

let rec getAttr<'Attr when 'Attr :> Attribute> (ty : Type) =
  if Attribute.IsDefined(ty, typeof<'Attr>) then
    (ty.GetCustomAttribute<'Attr>(), ty) |> Some
  else if ty.IsInterface then
    None
  else
    ty.GetInterfaces()
    |> Seq.choose (getAttr)
    |> Seq.tryFirst
    |> Option.tryIfNone (fun () -> if ty.BaseType <> null then getAttr ty.BaseType else None)

let rec depthFrom (rootTy : Type) (ty : Type) =
  if ty = null then failwith "Null type in depthFrom"
  if ty = rootTy then 0
  else (depthFrom rootTy ty.BaseType) + 1

let resolveDuplicateTypes (types : (TypeInfo * p4Attribute * Type) seq) =
  types
  |> Seq.groupBy (fun (ty,attr,defTy) -> attr.Type, attr.P4Path)
  |> Seq.collect (fun ((kP4Ty,kPath), tys) ->
      match kP4Ty with
      | p4Type.Error ->
          if kPath <> "error" then failwithf "P4Attribute cannot be of type Error and not have path 'error' (has path %s)." kPath
          // We want the deepest declaration
          tys |> Seq.maxBy (fst3 >> depthFrom libraryErrorType) |> Seq.singleton
      | _ -> tys)

let mapArchAssembly (archClassRequirement:ArchClassRequirement) (dll:Assembly) =
  // Search for types and members which have been annotated with the P4Attribute
  //  and for types annotated with P4LookupAttribute
  let warnings,lookups,types,arch,members =
    // Search public types
    dll.ExportedTypes
    |> Seq.map (fun ty -> ty.GetTypeInfo())
    |> Seq.fold (fun (warnings,lus,tys,arch,ms) ty ->
          // Add this type or its members if they have a p4 attribute, keeping a list of warnings
          let mutable warnings = warnings
          let addMessage message = warnings <- message::warnings
          let warn message = warnings <- (Warning message)::warnings // NOTE warnings are in reverse order
          let lus =
            if Attribute.IsDefined(ty, p4LookupAttributeType) then // We only check for the lookup attribute directly on a class
              // TODO check that this attribute is on a valid class
              // If it is an interface, is it an impl intf or one we generated? If not, we don't care unless in 'check' mode (when we will also want to check that every required att/impl is present)
              // If it is a class, does it implement the correct intf?
              //if ty.GetInterfaces() |> Seq.exists (fun intf -> intf. ...) then warn "Types annotated with P4LookupAttribute must implement ILookup<,>"
              ty::lus
            else lus
          let tys =
            match getAttr<p4Attribute> ty with
            | Some (attr, defTy) -> (ty, attr, defTy)::tys
            | None -> tys
          let arch =
            match archClassRequirement with
            | RequireArchClass ->
                match Attribute.IsDefined(ty, p4ArchitectureAttributeType), arch with
                | true, Some (existingArch:TypeInfo) ->
                    errorNowf warnings "There must only be one class annotated with P4ArchitectureAttribute. Found %s and %s." ty.Name existingArch.Name
                | true, None -> Some ty
                | false, arch -> arch
            | DontRequireArchClass -> arch
          let ms =
            let newMs =
              // We are interested in static methods and const/static-readonly fields/properties
              ty.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
              |> Seq.filter (fun m -> Attribute.IsDefined(m, p4AttributeType))
              |> List.ofSeq
            newMs @ ms
          (warnings, lus, tys, arch, ms)
        ) ([], [], [], None, [])
  let mutable warnings = warnings
  let addWarning warning = warnings <- warning::warnings
  let addWarnings warning = warnings <- warning@warnings

  let isArch : MemberInfo -> bool =
    match archClassRequirement, arch with
    | RequireArchClass, Some arch -> (=) (arch :> MemberInfo)
    | RequireArchClass, None ->  errorNowf warnings "No class annotated with P4ArchitectureAttribute was found. Exactly one such class must be publically declared."
    | DontRequireArchClass, _ -> fun _ -> true

  let rec isDefinedInArch (m : MemberInfo) =
    isArch m || isDefinedInArch m.DeclaringType

  // Create maps of p4-info -> c# name
  let p4Map =
    types
    |> resolveDuplicateTypes
    |> Seq.choose (fun (ty, attr, defTy) -> // FIXME use interface type (defTy) instead of concrete type
        let (verdict, warnings) = validCsForP4 attr (Type ty)
        addWarnings warnings
        let key = (attr.Type, attr.P4Path)
        let value =
          let tyName = ty.FullName.Replace('+', '.') // Nested members are ty+mem, we need ty.mem
          if ty.ContainsGenericParameters then
            tyName.Substring(0,  tyName.IndexOf('`')) // FIXME a bit hacky...
          else tyName
        match verdict with
        | Model -> Some (key, value)
        | Gen -> if isDefinedInArch ty then None else errorNowf warnings "Architecture members (%s) must be declared in the class annotated with P4ArchitectureAttribute" (ty.FullName)
        | Invalid -> None)
    |> Seq.append (members |> Seq.choose (fun m ->
        let attr = m.GetCustomAttribute<p4Attribute>()
        if attr = null then None
        else
          let (verdict, warnings) = validCsForP4 attr (Member m)
          addWarnings warnings
          let key = (attr.Type, attr.P4Path)
          let value = sprintf "%s.%s" m.DeclaringType.FullName m.Name
          let value = value.Replace('+', '.') // Nested members are ty+mem, we need ty.mem
          match verdict with
          | Model -> Some (key, value)
          | Gen -> if isDefinedInArch m then None else errorNowf warnings "Architecture members (%s) must be declared in the class annotated with P4ArchitectureAttribute" (m.Name)
          | Invalid -> None))
    |> Map.ofSeq
  // FIXME could check if declared match-kind actually matches a valid match_kind declared in this dll?
  let lookupMap =
    lookups
    |> Seq.map (fun lu ->
        let attr = lu.GetCustomAttribute<p4LookupAttribute>()
        let key = attr.MatchKind
        let value =
          let luName = lu.FullName.Replace('+', '.') // Nested members are ty+mem, we need ty.mem
          if lu.ContainsGenericParameters then
            luName.Substring(0,  luName.IndexOf('`')) // FIXME a bit hacky...
          else luName
        (key, value))
    |> Map.ofSeq
  let architectureClassName = arch |> Option.map (fun arch -> arch.FullName.Replace('+', '.')) |> Option.ifNoneValue ""
  (warnings, p4Map, lookupMap, architectureClassName)

let mapArchDll (filename:string) =
  let dll = Assembly.LoadFile(System.IO.FileInfo(filename).FullName)
  mapArchAssembly RequireArchClass dll

let getLibMap() =
  let (warnings, p4Map, lookupMap, arch) as rv =
    Assembly.GetAssembly(typeof<P4ToCSharp.Library.LpmTable<_,_>>) // Get Library dll
    |> mapArchAssembly DontRequireArchClass
  assert warnings.IsEmpty
  lookupMap
