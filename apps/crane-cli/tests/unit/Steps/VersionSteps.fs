module CraneCli.Tests.Unit.Steps.VersionSteps

open System.Reflection
open System.Text.RegularExpressions
open TickSpec
open Xunit

let mutable private versionString: string = ""

[<When>]
let ``I read the assembly version`` () =
    let asm = Assembly.GetAssembly(typeof<CraneCli.Models.Finding.Finding>)
    let v = asm.GetName().Version
    versionString <- if isNull v then "0.0.0" else v.ToString()

[<Then>]
let ``the version string matches a SemVer-shaped pattern`` () =
    Assert.Matches(@"^\d+\.\d+\.\d+(\.\d+)?$", versionString)
