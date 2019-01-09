namespace Freya.Types.Uri.Template.Runtime

open System
open Freya.Types.Uri.Template
open System.Reflection

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Freya.Types.Uri.Template.Provider.DesignTime.dll")>]
do ()
