module CraneCli.Models.PdfMetadata

open System.Text.Json.Serialization

type PdfMetadata =
    { [<JsonPropertyName("pages")>]
      Pages: int
      [<JsonPropertyName("title")>]
      Title: string option
      [<JsonPropertyName("author")>]
      Author: string option
      [<JsonPropertyName("file")>]
      File: string }
