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

type DllInfo = Assembly

// Short names for attribute types
type p4Type = P4ToCSharp.Library.P4Type
type p4Attribute = P4ToCSharp.Library.P4Attribute
type p4LookupAttribute = P4ToCSharp.Library.P4LookupAttribute
let p4AttributeType = typeof<p4Attribute>
let p4LookupAttributeType = typeof<p4LookupAttribute>

type AnnotationVerdict =
  | Gen // Generated ONLY for the purpose of the interface
  | Model // Actually used as implementation
  | Invalid // Not valid
type TypeOrMember = Type of Type | Member of MemberInfo
  with
  member this.CategoryString = match this with Type _ -> "type" | Member _ -> "member"
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
  | (p4Type.Enum | p4Type.Error | p4Type.MatchKind), Type ty ->
      if not (ty.IsEnum) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate an enum." p4.P4Path p4.Type ])
      else (Model, [])
  // Classes
  | (p4Type.ExternObject), Type ty ->
      if not (ty.IsClass) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a class or struct." p4.P4Path p4.Type ])
      else (Model, [])
  // Non-static classes
  | (p4Type.Header | p4Type.Package | p4Type.Struct), Type ty ->
      if not (ty.IsClass && not (ty.IsAbstract && ty.IsSealed)) then (Invalid, [ warningf "P4Attribute(\"%s\", %A) must annotate a non-static class." p4.P4Path p4.Type ])
      else (Model, [])
  // Should be Type/Member, but is Member/Type
  | _ -> (Invalid, [ warningf "P4Attribute(\"%s\", %A) cannot annotate a %s." p4.P4Path p4.Type cs.CategoryString ])

let mapArchDll (filename:string) =
  let dll = Assembly.LoadFile(filename)
  // Search for types and members which have been annotated with the P4Attribute
  //  and for types annotated with P4LookupAttribute
  let warnings,lookups,types,members =
    // Search public types
    dll.ExportedTypes
    |> Seq.fold (fun (warnings,lus,tys,ms) ty ->
          // Add this type or its members if they have a p4 attribute, keeping a list of warnings
          let mutable warnings = warnings
          let warn message = warnings <- (Warning message)::warnings // NOTE warnings are in reverse order
          let lus =
            if Attribute.IsDefined(ty, p4LookupAttributeType) then
              // TODO check that this attribute is on a valid class
              // If it is an interface, is it an impl intf or one we generated? If not, we don't care unless in 'check' mode (when we will also want to check that every required att/impl is present)
              // If it is a class, does it implement the correct intf?
              //if ty.GetInterfaces() |> Seq.exists (fun intf -> intf. ...) then warn "Types annotated with P4LookupAttribute must implement ILookup<,>"
              ty::lus
            else lus
          let tys =
            if Attribute.IsDefined(ty, p4AttributeType) then ty::tys else tys
          let ms =
            let newMs =
              // We are interested in static methods and const/static-readonly fields/properties
              ty.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
              |> Seq.filter (fun meth -> Attribute.IsDefined(ty, p4AttributeType))
              |> List.ofSeq
            newMs @ ms
          (warnings, lus, tys, ms)
        ) ([], [], [], [])
  let mutable warnings = warnings
  let addWarning warning = warnings <- warning::warnings
  let addWarnings warning = warnings <- warning@warnings

  // Create maps of p4-info -> c# name
  let p4Map =
    types
    |> Seq.choose (fun ty ->
        let attr = ty.GetCustomAttribute<p4Attribute>()
        let (verdict, warnings) = validCsForP4 attr (Type ty)
        addWarnings warnings
        let key = (attr.Type, attr.P4Path)
        let value = ty.FullName
        match verdict with
        | Model -> Some (key, value)
        | Gen | Invalid -> None)
    |> Seq.append (members |> Seq.choose (fun m ->
        let attr = m.GetCustomAttribute<p4Attribute>()
        let (verdict, warnings) = validCsForP4 attr (Member m)
        addWarnings warnings
        let key = (attr.Type, attr.P4Path)
        let value = sprintf "%s.%s" m.DeclaringType.FullName m.Name
        match verdict with
        | Model -> Some (key, value)
        | Gen | Invalid -> None))
    |> Map.ofSeq
  // FIXME could check if declared match-kind actually matches a valid match_kind declared in this dll?
  let lookupMap =
    lookups
    |> Seq.map (fun lu ->
        let attr = lu.GetCustomAttribute<p4LookupAttribute>()
        let key = attr.MatchKind
        let value = lu.FullName
        (key, value))
    |> Map.ofSeq
  (warnings, p4Map, lookupMap)
