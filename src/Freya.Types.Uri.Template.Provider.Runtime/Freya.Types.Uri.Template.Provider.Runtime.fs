namespace Freya.Types.Uri.Template.Runtime

open System
open Freya.Types.Uri.Template
open System.Reflection

module RuntimeHelpers = 
    let getTemplateString (ty: Type) =
        // would ideally be private
        ty.GetField("template", (* BindingFlags.NonPublic ||| *) BindingFlags.Static).GetValue(null) :?> string

    let getUriTemplate (ty: Type) = 
        ty.GetProperty("Template", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) :?> UriTemplate

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Freya.Types.Uri.Template.Provider.DesignTime.dll")>]
do ()
