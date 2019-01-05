module Freya.Types.Uri.Template.ProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Freya.Types.Uri.Template
open System.Diagnostics
open ProviderImplementation.ProvidedTypes

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

    let tryAddTemplateParameter template (ty: ProvidedTypeDefinition) =

        let makeTemplateMember (template: UriTemplate) =
            let m = ProvidedProperty("Template", typeof<UriTemplate>, getterCode = (fun _ -> Expr.Value(template, typeof<UriTemplate>)))
            m.AddXmlDoc (sprintf "The parsed UriTemplate for the source string.'%O'" template)
            m
        
        let makeRenderMember (template: UriTemplate) = 
            let m = ProvidedProperty("Render", typeof<UriTemplateData -> string>, (fun _ -> <@@ template.Render @@>), isStatic = true)
            m.AddXmlDoc (sprintf "Render the template '%O' with a data bundle." template)
            m

        let makeMatchMember (template: UriTemplate) = 
            let m = ProvidedProperty("Match", typeof<string -> UriTemplateData>, (fun _ -> <@@ template.Match @@>), isStatic = true)
            m.AddXmlDoc (sprintf "Render the template '%O' with a data bundle." template)
            m

        match UriTemplate.tryParse template with
        | Ok template ->
            ty.AddMember (makeTemplateMember template)
            ty.AddMember (makeRenderMember template)
            ty.AddMember (makeMatchMember template)
            ty
        | Error parseError ->
            failwithf "Error parsing template '%s':\n\t%s" template parseError

    let addTypeDocs template (providedType: ProvidedTypeDefinition) =
        providedType.AddXmlDoc (sprintf "A typechecked version of the URI Template '%s'" template)
        providedType

    let createTemplateType (typeName: string) ([| StringParam uriTemplate |]) =
        try 
            let innerAssembly = ProvidedAssembly()
            let ty = 
                ProvidedTypeDefinition(innerAssembly, ``namespace``, typeName, Some typeof<RuntimeContext>, isErased = false)
                |> tryAddTemplateParameter uriTemplate
                |> addTypeDocs uriTemplate
            innerAssembly.AddTypes [ty]
            ty
        with e -> 
            File.AppendAllText("/Users/chethusk/compilelog.log", 
                sprintf """error while creating type: %s\n\t:%s""" e.Message e.StackTrace
            )
            reraise ()

    let providerType =
        let ty = ProvidedTypeDefinition(hostAssembly, ``namespace``, "TemplateProvider", Some typeof<obj>, isErased = false)
        ty.DefineStaticParameters(
            [requiredStaticParameter<string> "template" "The URI Template (per the [URI Template spec](https://tools.ietf.org/html/rfc6570)) for which to generate a type"],
            createTemplateType
        )
        ty

    do
        try 
            this.AddNamespace(``namespace``, [ providerType ])        
        with e -> 
            File.AppendAllText("/Users/chethusk/compilelog.log", 
                sprintf """error while creating type: %s\n\t:%s""" e.Message e.StackTrace
            )
            reraise ()

[<TypeProviderAssembly>]
do
  ProvidedTypeDefinition.Logger.Value <- Some (fun str -> File.AppendAllText("/Users/chethusk/compilelog.log", str))
