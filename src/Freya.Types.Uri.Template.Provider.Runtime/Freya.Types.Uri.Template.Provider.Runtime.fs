namespace Freya.Types.Uri.Template.Runtime

open System
open Freya.Types.Uri.Template
open System.Reflection
open System.IO

module Logging =
    let logfn format =
        Printf.kprintf (fun s -> File.AppendAllText("/Users/chethusk/compile.log", sprintf "[%s]:\t%s%s" (DateTime.Now.ToLongTimeString()) s Environment.NewLine)) format


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Freya.Types.Uri.Template.Provider.DesignTime.dll")>]
do ()
