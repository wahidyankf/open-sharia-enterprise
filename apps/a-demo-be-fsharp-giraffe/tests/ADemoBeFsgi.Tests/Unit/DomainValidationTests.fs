module ADemoBeFsgi.Tests.Unit.DomainValidationTests

open Xunit
open ADemoBeFsgi.Domain.Types
open ADemoBeFsgi.Domain.User
open ADemoBeFsgi.Domain.Expense
open ADemoBeFsgi.Domain.Attachment

[<Trait("Category", "Unit")>]
type DomainValidationTests() =

    // Password validation
    [<Fact>]
    member _.``Valid password passes validation``() =
        let result = validatePassword "Str0ng#Pass1!"
        Assert.True(Result.isOk result)

    [<Fact>]
    member _.``Empty password fails validation``() =
        let result = validatePassword ""
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Short password fails validation``() =
        let result = validatePassword "Short1!"
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Password without uppercase fails``() =
        let result = validatePassword "str0ng#pass1!"
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Password without special char fails``() =
        let result = validatePassword "AllUpperCase1234"
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Password without digit fails``() =
        let result = validatePassword "Strong#PassNoDigit"
        Assert.True(Result.isError result)

    // Email validation
    [<Fact>]
    member _.``Valid email passes``() =
        let result = validateEmail "alice@example.com"
        Assert.True(Result.isOk result)

    [<Fact>]
    member _.``Invalid email fails``() =
        let result = validateEmail "not-an-email"
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Empty email fails``() =
        let result = validateEmail ""
        Assert.True(Result.isError result)

    // Username validation
    [<Fact>]
    member _.``Valid username passes``() =
        let result = validateUsername "alice"
        Assert.True(Result.isOk result)

    [<Fact>]
    member _.``Short username fails``() =
        let result = validateUsername "ab"
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``Empty username fails``() =
        let result = validateUsername ""
        Assert.True(Result.isError result)

    // Currency parsing
    [<Fact>]
    member _.``Parse USD succeeds``() =
        Assert.Equal(Ok USD, parseCurrency "USD")

    [<Fact>]
    member _.``Parse IDR succeeds``() =
        Assert.Equal(Ok IDR, parseCurrency "IDR")

    [<Fact>]
    member _.``Parse EUR fails``() =
        Assert.True(Result.isError (parseCurrency "EUR"))

    // Amount validation
    [<Fact>]
    member _.``Positive amount passes``() =
        Assert.True(Result.isOk (validateAmount 10.50m))

    [<Fact>]
    member _.``Negative amount fails``() =
        Assert.True(Result.isError (validateAmount -1m))

    [<Fact>]
    member _.``Zero amount passes``() =
        Assert.True(Result.isOk (validateAmount 0m))

    // Currency precision
    [<Fact>]
    member _.``USD with 2 decimals passes``() =
        Assert.True(Result.isOk (validateCurrencyPrecision "USD" 10.50m))

    [<Fact>]
    member _.``IDR whole number passes``() =
        Assert.True(Result.isOk (validateCurrencyPrecision "IDR" 150000m))

    // Unit validation
    [<Fact>]
    member _.``Valid unit passes``() =
        Assert.True(Result.isOk (validateUnit (Some "liter")))

    [<Fact>]
    member _.``Invalid unit fails``() =
        Assert.True(Result.isError (validateUnit (Some "fathom")))

    [<Fact>]
    member _.``No unit passes``() =
        Assert.True(Result.isOk (validateUnit None))

    // Attachment validation
    [<Fact>]
    member _.``JPEG content type passes``() =
        Assert.True(Result.isOk (validateContentType "image/jpeg"))

    [<Fact>]
    member _.``Unsupported content type fails``() =
        Assert.True(Result.isError (validateContentType "application/octet-stream"))

    [<Fact>]
    member _.``File within size limit passes``() =
        Assert.True(Result.isOk (validateFileSize (5L * 1024L * 1024L)))

    [<Fact>]
    member _.``Oversized file fails``() =
        Assert.True(Result.isError (validateFileSize (11L * 1024L * 1024L)))
