module DemoBeFsgi.Domain.Attachment

open System
open DemoBeFsgi.Domain.Types

type Attachment =
    { Id: Guid
      ExpenseId: Guid
      Filename: string
      ContentType: string
      FileSize: int64
      Data: byte[]
      Url: string
      CreatedAt: DateTime }

let supportedContentTypes = set [ "image/jpeg"; "image/png"; "application/pdf" ]

let maxFileSize = 10L * 1024L * 1024L // 10MB

let validateContentType (contentType: string) =
    if supportedContentTypes.Contains(contentType) then
        Ok contentType
    else
        Error(UnsupportedMediaType $"Unsupported content type: %s{contentType}")

let validateFileSize (size: int64) =
    if size > maxFileSize then
        Error(FileTooLarge maxFileSize)
    else
        Ok size
