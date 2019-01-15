module Freya.Types.Uri.Template.ProviderTests

open NUnit.Framework
open Freya.Types.Uri.Template
open Freya.Types.Uri.Template.Obsolete
open Freya.Types.Uri.Template.Obsolete
open Freya.Types.Uri.Template.Obsolete

type SimpleTemplate = TemplateProvider<"/foo">

[<Test>]
let ``Can access simple uri template with prop`` () = 
    let rendered = SimpleTemplate.Render (UriTemplateData.UriTemplateData Map.empty)
    Assert.AreEqual(rendered, "/foo")

    let matchData: UriTemplateData = SimpleTemplate.Match("/foo")
    Assert.AreEqual(UriTemplateData.UriTemplateData Map.empty, matchData)

type QueryTemplate = TemplateProvider<"/find{?year*}">

[<Test>]
let ``Can create render method for complex template`` () =
    let data = [Key "year", UriTemplateValue.List ["1982"; "1983"]] |> Map.ofList |> UriTemplateData
    let rendered = QueryTemplate.Render data
    Assert.AreEqual(rendered, "/find?year=1982&year=1983")

    let matchData = QueryTemplate.Match("/find?year=1982&year=1983")
    Assert.AreEqual(matchData, data)