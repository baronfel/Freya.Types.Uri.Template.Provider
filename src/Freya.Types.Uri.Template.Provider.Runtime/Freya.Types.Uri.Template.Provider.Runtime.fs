namespace MyNamespace

open System
open Freya.Types.Uri.Template

// Put any runtime constructs here
type RuntimeContext (template: UriTemplate) =
    member __.Template = template
    member __.Render data = template.Render data
    member __.Match uri = template.Match uri
    override __.ToString() = string template

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Freya.Types.Uri.Template.Provider.DesignTime.dll")>]
do ()
