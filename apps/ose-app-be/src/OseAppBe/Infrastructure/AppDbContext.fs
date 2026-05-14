module OseAppBe.Infrastructure.AppDbContext

open Microsoft.EntityFrameworkCore

type AppDbContext(options: DbContextOptions<AppDbContext>) =
    inherit DbContext(options)
// DbSet<> declarations added in feature plans when entities are defined
