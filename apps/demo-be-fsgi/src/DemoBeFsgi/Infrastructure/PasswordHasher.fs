module DemoBeFsgi.Infrastructure.PasswordHasher

let hashPassword (password: string) : string =
    BCrypt.Net.BCrypt.HashPassword(password)

let verifyPassword (password: string) (hash: string) : bool =
    BCrypt.Net.BCrypt.Verify(password, hash)
