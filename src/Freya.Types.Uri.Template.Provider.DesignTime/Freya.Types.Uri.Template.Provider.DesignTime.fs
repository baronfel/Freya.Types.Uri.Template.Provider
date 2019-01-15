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

    let addMembers (template: string) (ty: ProvidedTypeDefinition) = // (renderTy: ProvidedTypeDefinition) =    
        match UriTemplate.tryParse template with
        | Ok uriTemplate ->
            let templateField = ProvidedField("Template", typeof<UriTemplate>)
            templateField.SetFieldAttributes FieldAttributes.Static
            templateField.AddXmlDoc (sprintf "The parsed UriTemplate for the source string.'%s'" template)
            ty.AddMember templateField
            
            let templateExpr = <@ template @>
            let ensureTemplateField = 
                <@ 
                    let uriTemplate = (%%Expr.FieldGet templateField : UriTemplate)
                    if isNull (box uriTemplate) then
                        Logging.logfn "Initializing field"
                        let template' = UriTemplate.parse %templateExpr
                        Logging.logfn "Got template"
                        %%Expr.FieldSet(templateField, <@ UriTemplate.parse template @>.Raw)

                    (%%Expr.FieldGet templateField : UriTemplate)
                @>
            Logging.logfn "Created backing field"
            
            // let renderTy = 
            //     let fnTy = typedefof<string -> bool>
            //     fnTy.MakeGenericType [| renderTy :> Type; typeof<string> |]

            let renderProp = ProvidedProperty("Render",
                                              typeof<UriTemplateData -> string>,
                                              getterCode = (fun _ -> <@ (%ensureTemplateField).Render @>.Raw), 
                                              isStatic = true)
            renderProp.AddXmlDoc (sprintf "Render the template '%s' with a data bundle." template)
            ty.AddMember renderProp
            Logging.logfn "Made Render prop"

            let matchProp = ProvidedProperty("Match",
                                             typeof<string -> UriTemplateData>,
                                             getterCode = (fun _ -> <@ (%ensureTemplateField).Match @>.Raw),
                                             isStatic = true)
            matchProp.AddXmlDoc (sprintf "Get the match data for the route '%s'" template)
            ty.AddMember matchProp
            Logging.logfn "Made Match prop"
            ty
        | Error parseError ->
            Logging.logfn "Error parsing template '%s':\n\t%s" template parseError
            failwithf "Error parsing template '%s':\n\t%s" template parseError

    let addTypeDocs template (providedType: ProvidedTypeDefinition) =
        providedType.AddXmlDoc (sprintf "A typechecked version of the URI Template '%s'" template)
        providedType

    let createTemplateType (typeName: string) ([| StringParam uriTemplate |]) =
        let innerAssembly = ProvidedAssembly()
        Logging.logfn "making type %s" typeName
        let templateTy = ProvidedTypeDefinition(innerAssembly, ``namespace``, typeName, Some typeof<obj>, isErased = false)
        //let renderTy = createRenderType uriTemplate 
        let ty =
            templateTy
            |> addMembers uriTemplate// renderTy
            |> addTypeDocs uriTemplate
        //ty.AddMember renderTy
        innerAssembly.AddTypes [ty]
        ty

    do
        try
            let providerType =
                let ty = ProvidedTypeDefinition(hostAssembly, ``namespace``, "TemplateProvider", Some typeof<obj>, isErased = false)
                ty.DefineStaticParameters(
                    [requiredStaticParameter<string> "template" "The URI Template (per the [URI Template spec](https://tools.ietf.org/html/rfc6570)) for which to generate a type"],
                    createTemplateType
                )
                ty

            this.AddNamespace(``namespace``, [ providerType ])
        with e ->
            Logging.logfn """error while creating type: %s\n\t:%s""" e.Message e.StackTrace
            reraise ()

[<TypeProviderAssembly>]
do
  ProvidedTypeDefinition.Logger.Value <- Some (fun str -> File.AppendAllText("/Users/chethusk/compilelog.log", str))
