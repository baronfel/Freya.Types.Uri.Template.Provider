module Freya.Types.Uri.Template.ProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Freya.Types.Uri.Template
open Freya.Types.Uri.Template.Runtime
open System.Diagnostics
open ProviderImplementation.ProvidedTypes
open Freya.Types.Uri.Template.Obsolete
open Freya.Types.Uri.Template.Obsolete

// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =

    let inline requiredStaticParameter<'t> name docString =
        let p = ProvidedStaticParameter(name, typeof<'t>, None)
        p.AddXmlDoc docString
        p
    let inline optionalStaticParameter name (value: 't) docString =
        let p = ProvidedStaticParameter(name, typeof<'t>, Some (box value))
        p.AddXmlDoc docString
        p

    let inline (|StringParam|_|) (s: obj) =
        match s with
        | :? string as s -> Some s
        | _ -> None

[<TypeProvider>]
type FreyaUriTemplateProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("Freya.Types.Uri.Template.Provider.DesignTime", "Freya.Types.Uri.Template.Provider.Runtime")], addDefaultProbingLocation=true)

    let ``namespace`` = "Freya.Types.Uri.Template"
    let hostAssembly = Assembly.GetExecutingAssembly()

    //TODO: check we contain a copy of runtime files, and are not referencing the runtime DLL
    
    /// Create a type that will be used to make `Render` typed, by inspecting the URI template provided and 
    /// making a type whose properties are the named segments of the template.
    let createRenderType (template: string) = 
        match UriTemplate.tryParse template with
        | Ok uriTemplate -> 
            let variableParts = 
                let getSpecName (spec: VariableSpec) = 
                    match spec with
                    | VariableSpec(VariableName name, modifier) -> name

                let getListNames (specs: VariableSpec list) = 
                    match specs with
                    | [] -> None
                    | specs -> specs |> List.map getSpecName |> Some

                let getNamesFromExpression (Expression(operator, variables)) = 
                    match variables with
                    | VariableList list -> getListNames list

                let getPartNames (part: UriTemplatePart) = 
                    match part with
                    | UriTemplatePart.Literal _ -> None
                    | UriTemplatePart.Expression e -> getNamesFromExpression e
                match uriTemplate with
                | UriTemplate.UriTemplate parts -> parts |> List.choose getPartNames |> List.collect id

            let renderTy = ProvidedTypeDefinition("RouteParameters", baseType = None, hideObjectMethods = true, nonNullable = true, isErased = false, isSealed = true)
            for name in variableParts do
                renderTy.AddMember(ProvidedField.Literal(name, typeof<string>, name))
            renderTy.AddMember (ProvidedConstructor([], (fun _ -> <@ () @>.Raw)))
            Logging.logfn "Made RouteParameters type"
            renderTy
        | Error parseError ->
            Logging.logfn "Error parsing template '%s':\n\t%s" template parseError
            failwith (string parseError)

    /// Creates the backing storage field for the `UriTemplate` instance represented by the template `template`
    /// This field is attached to the type and a getter is returned for external usage.
    /// In an ideal world I'd find a way to set this value in a static constructor, so that we could skip the `isNull` check
    let makeBackingField template (ty: ProvidedTypeDefinition): Expr<UriTemplate> =
        let templateField = ProvidedField("Template", typeof<UriTemplate>)
        templateField.SetFieldAttributes FieldAttributes.Static
        templateField.AddXmlDoc (sprintf "The parsed UriTemplate for the source string.'%s'" template)
        ty.AddMember templateField
        Logging.logfn "Created backing field"

        <@ 
            let uriTemplate = (%%Expr.FieldGet templateField : UriTemplate)
            if isNull (box uriTemplate) then
                Logging.logfn "Initializing field"
                %%Expr.FieldSet(templateField, <@ UriTemplate.parse template @>.Raw)
            (%%Expr.FieldGet templateField : UriTemplate)
        @>

    /// Creates a property called `Render` on the provided type. 
    /// This property will take some input route data and return the route template populated with that data as a string
    let createRenderProperty template (getTemplateExpr: Expr<UriTemplate>) (ty: ProvidedTypeDefinition) = 
        let renderProp = ProvidedProperty("Render",
                                          typeof<UriTemplateData -> string>,
                                          getterCode = (fun _ -> <@ (%getTemplateExpr).Render @>.Raw), 
                                          isStatic = true)
        renderProp.AddXmlDoc (sprintf "Render the template '%s' with a data bundle." template)
        ty.AddMember renderProp
        Logging.logfn "Made Render prop"

    /// Creates a property called `Match` on the provided type.
    /// This property will take an input route and determine if a given route satisfies the route template.
    let createMatchProperty template (getTemplateExpr: Expr<UriTemplate>) (ty: ProvidedTypeDefinition) =
        let matchProp = ProvidedProperty("Match",
                                         typeof<string -> UriTemplateData>,
                                         getterCode = (fun _ -> <@ (%getTemplateExpr).Match @>.Raw),
                                         isStatic = true)
        matchProp.AddXmlDoc (sprintf "Get the match data for the route '%s'" template)
        ty.AddMember matchProp
        Logging.logfn "Made Match prop"

    /// Parse the route template, and if it is a valid route generate the type for the checked helper properties
    let addMembers (template: string) (ty: ProvidedTypeDefinition) = // (renderTy: ProvidedTypeDefinition) =    
        match UriTemplate.tryParse template with
        | Ok uriTemplate ->
            let backingField = makeBackingField template ty            
            createRenderProperty template backingField ty
            createMatchProperty template backingField ty

        | Error parseError ->
            Logging.logfn "Error parsing template '%s':\n\t%s" template parseError
            failwithf "Error parsing template '%s':\n\t%s" template parseError
    
    /// Try to create a type from the static parameters provided to us.
    let createTemplateType (typeName: string) ([| StringParam uriTemplate |]) =
        // We must make a dummy assembly for our types to live in.
        // This types written to the assembly will be injected into the Assembly that calls the type provider
        let innerAssembly = ProvidedAssembly()
        Logging.logfn "making type %s" typeName
        // We're making a Generated Type whose name is based off of the `type XXXX = TemplateProvide<"">` expression that the user used.
        let templateType = ProvidedTypeDefinition(innerAssembly, ``namespace``, typeName, Some typeof<obj>, isErased = false)
        templateType.AddXmlDoc (sprintf "A typechecked version of the URI Template '%s'" uriTemplate)
        addMembers uriTemplate templateType
        // once our types are structured we must add them to the parent assembly, or else thye won't be discovered
        innerAssembly.AddTypes [templateType]
        templateType

    let providerType =
        let ty = ProvidedTypeDefinition(hostAssembly, ``namespace``, "TemplateProvider", Some typeof<obj>, isErased = false)
        ty.DefineStaticParameters(
            [requiredStaticParameter<string> "template" "The URI Template (per the [URI Template spec](https://tools.ietf.org/html/rfc6570)) for which to generate a type"],
            createTemplateType
        )
        ty
    
    do
        this.AddNamespace(``namespace``, [ providerType ])

[<TypeProviderAssembly>]
do ()
