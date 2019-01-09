module Freya.Types.Uri.Template.ProviderTests

open NUnit.Framework
open Freya.Types.Uri.Template
open Freya.Types.Uri.Template.Obsolete

type SimpleTemplate = TemplateProvider<"/foo">

[<Test>]
let ``Can access simple uri template with prop`` () = 
    let tmpl = SimpleTemplate.template |> UriTemplate.parse
    Assert.AreEqual(tmpl, SimpleTemplate.Template)
    let rendered = SimpleTemplate.Render (UriTemplateData.UriTemplateData Map.empty)
    Assert.AreEqual(rendered, "/foo")

    let matchData: UriTemplateData = SimpleTemplate.Match("/foo")
    Assert.AreEqual(UriTemplateData.UriTemplateData Map.empty, matchData)

