import Link from "next/link";
import { Button } from "@open-sharia-enterprise/ts-ui";

export function Hero() {
  return (
    <section className="mx-auto max-w-screen-xl px-4 py-16 sm:py-24">
      <div className="mx-auto max-w-2xl text-center">
        <h1 className="text-4xl font-bold tracking-tight sm:text-5xl">Open Sharia Enterprise Platform</h1>
        <p className="mt-4 text-lg text-muted-foreground">
          <strong>Built in the Open &middot; Sharia-Compliant &middot; Enterprise-Ready</strong>
        </p>
        <p className="mt-4 text-muted-foreground">
          Open-source platform for Sharia-compliant enterprise solutions. Starting with individual productivity tools,
          expanding to MSME, then enterprise.
        </p>
        <div className="mt-8 flex flex-col items-center justify-center gap-3 sm:flex-row">
          <Button asChild>
            <Link href="/about/">Learn More &rarr;</Link>
          </Button>
          <Button variant="outline" asChild>
            <a href="https://github.com/wahidyankf/open-sharia-enterprise" target="_blank" rel="noopener noreferrer">
              GitHub &rarr;
            </a>
          </Button>
        </div>
      </div>

      <div className="mx-auto mt-16 max-w-xl">
        <h2 className="text-xl font-semibold">Why Open Source?</h2>
        <ul className="mt-4 space-y-3 text-muted-foreground">
          <li className="flex gap-2">
            <span className="font-semibold text-foreground">Transparency &amp; Trust</span> &mdash; Full visibility into
            compliance implementation and security practices
          </li>
          <li className="flex gap-2">
            <span className="font-semibold text-foreground">Accessibility</span> &mdash; Free for organizations of all
            sizes to use, customize, and deploy
          </li>
          <li className="flex gap-2">
            <span className="font-semibold text-foreground">Community-Driven</span> &mdash; Built with input from
            developers, Islamic finance experts, and enterprise users
          </li>
        </ul>
        <p className="mt-6 text-sm text-muted-foreground">Currently in pre-alpha development.</p>
      </div>
    </section>
  );
}
