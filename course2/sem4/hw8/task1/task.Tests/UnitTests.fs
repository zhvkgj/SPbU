module Tests

open Hw8
open ParsingPage
open NUnit.Framework
open FsUnit

[<Test>]
let ``Test for getting references from href``() =
       getRefFromHref "<a href=\"http://fsharp.org/guides/slack/\" target=\"_blank\" title=\"F# Web Slack\">" 
       |> should equal "http://fsharp.org/guides/slack/"
       getRefFromHref "<a href=\"http://twitter.com/websharper\" class=\"button is-info\" style=\"background:#1b95e0\">"
       |> should equal "http://twitter.com/websharper"

    