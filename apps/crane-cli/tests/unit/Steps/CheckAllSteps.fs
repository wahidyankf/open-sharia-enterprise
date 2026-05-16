module CraneCli.Tests.Unit.Steps.CheckAllSteps

open TickSpec
open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.CheckAllCommands
open CraneCli.Tests.Unit.Steps.BddState

let mutable private pdfText: string = ""
let mutable private mdText: string = ""

[<Given>]
let ``a PDF fixture and an MD that matches across all dimensions`` () =
    let body =
        "Hello world section content. "
        + "This document covers introduction, scope, and requirements. "
        + "Each section is fully present and accurately transcribed."

    pdfText <- body
    mdText <- "# Title\n\n" + body + "\n"

[<Given>]
let ``a PDF fixture and an MD missing content`` () =
    pdfText <-
        "Critical missing section content goes here. "
        + "Important paragraph that the MD lacks entirely. "
        + "Another full passage that must not be dropped."

    mdText <- "# Title\n\nUnrelated short text.\n"

[<When>]
let ``I run "crane check-all" on the pair`` () =
    let fakeAdapter = FakePdfAdapter(pdfText, 1, 1024L) :> IPdfAdapter
    RunWithWriter(fun w -> runCheckAll fakeAdapter "fake.pdf" mdText w)
