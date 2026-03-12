using DemoBeCsas.Domain;
using DemoBeCsas.Infrastructure.Models;
using Microsoft.EntityFrameworkCore;

namespace DemoBeCsas.Infrastructure;

public class AppDbContext(DbContextOptions<AppDbContext> options) : DbContext(options)
{
    public DbSet<UserModel> Users => Set<UserModel>();
    public DbSet<ExpenseModel> Expenses => Set<ExpenseModel>();
    public DbSet<AttachmentModel> Attachments => Set<AttachmentModel>();
    public DbSet<RevokedTokenModel> RevokedTokens => Set<RevokedTokenModel>();

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<UserModel>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.HasIndex(e => e.Username).IsUnique();
            entity.HasIndex(e => e.Email).IsUnique();
            entity.Property(e => e.Status).HasConversion(
                v => v.ToString().ToUpperInvariant(),
                v => Enum.Parse<UserStatus>(v, ignoreCase: true)
            );
            entity.Property(e => e.Role).HasConversion(
                v => v.ToString().ToUpperInvariant(),
                v => Enum.Parse<Role>(v, ignoreCase: true)
            );
        });

        modelBuilder.Entity<ExpenseModel>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.Property(e => e.Amount).HasPrecision(18, 6);
            entity.Property(e => e.Type).HasConversion(
                v => v.ToString().ToUpperInvariant(),
                v => Enum.Parse<ExpenseType>(v, ignoreCase: true)
            );
        });

        modelBuilder.Entity<AttachmentModel>(entity =>
        {
            entity.HasKey(e => e.Id);
        });

        modelBuilder.Entity<RevokedTokenModel>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.HasIndex(e => e.Jti).IsUnique();
            entity.HasIndex(e => e.UserId);
        });
    }
}
