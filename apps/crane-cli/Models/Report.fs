module CraneCli.Models.Report

open System.Text.Json.Serialization

type SkipListEntry =
    { [<JsonPropertyName("md_basename")>]
      MdBasename: string
      [<JsonPropertyName("category")>]
      Category: string
      [<JsonPropertyName("description")>]
      Description: string
      [<JsonPropertyName("key")>]
      Key: string }
