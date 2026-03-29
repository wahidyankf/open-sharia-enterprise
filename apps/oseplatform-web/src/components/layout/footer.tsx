export function Footer() {
  const year = new Date().getFullYear();

  return (
    <footer className="border-t border-border py-6">
      <div className="mx-auto flex max-w-screen-xl flex-col items-center gap-2 px-4 text-sm text-muted-foreground sm:flex-row sm:justify-between">
        <p>
          &copy; {year} Open Sharia Enterprise Platform &middot;{" "}
          <a
            href="https://github.com/wahidyankf/open-sharia-enterprise/blob/main/LICENSE"
            target="_blank"
            rel="noopener noreferrer"
            className="hover:text-foreground"
          >
            MIT License
          </a>
        </p>
        <a href="https://ayokoding.com" target="_blank" rel="noopener noreferrer" className="hover:text-foreground">
          AyoKoding
        </a>
      </div>
    </footer>
  );
}
