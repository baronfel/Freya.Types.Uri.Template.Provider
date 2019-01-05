module Freya.Types.Uri.Template.ProviderTests

open NUnit.Framework
open Freya.Types.Uri.Template

type SimpleTemplate = TemplateProvider<"/foo">

[<Test>]
let ``Can access simple uri template with prop`` () = 
    let tmpl = SimpleTemplate.Template
    let rendered = tmpl.Render (UriTemplateData.UriTemplateData Map.empty)
    Assert.AreEqual(rendered, "/foo")

