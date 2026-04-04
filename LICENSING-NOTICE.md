# Licensing Notice

This repository is licensed under the **Functional Source License, Version 1.1, MIT Future License
(FSL-1.1-MIT)**. This notice provides a human-readable summary — see [LICENSE](./LICENSE) for the
full legal text.

## What Is FSL-1.1-MIT?

FSL-1.1-MIT is a source-available license created by [Sentry](https://blog.sentry.io/lets-talk-about-open-source/).
It grants broad rights to use, copy, modify, and redistribute the software for any purpose
**except** offering a competing commercial product or service. After 2 years, the code
automatically converts to the MIT license with no restrictions.

## What You Can Do

- Read, study, and learn from the source code
- Use the software internally for any purpose (including commercial)
- Modify the software for internal use
- Distribute the software (with the FSL license intact)
- Contribute back to the project
- Build non-competing products and services using the code
- Use the software for education, research, and evaluation
- Use individual libraries or components in unrelated projects

## What You Cannot Do

- Offer a commercial product or service that competes with this platform (specifically: a
  Sharia-compliant enterprise platform with substantially similar functionality) — **this
  restriction expires after 2 years**

## Per-Version Rolling Conversion to MIT

The FSL-1.1-MIT license converts to MIT on a **per-version (per-commit) rolling basis**, not as a
single blanket date:

- Each version of the software becomes MIT-licensed **2 years after its first public
  distribution**
- Code committed on 2026-04-04 becomes MIT on 2028-04-04
- Code committed on 2026-06-15 becomes MIT on 2028-06-15
- Code committed on 2027-03-20 becomes MIT on 2029-03-20

This means older code progressively becomes fully open-source (MIT) over time, while new code is
always protected for 2 years from its publication date.

## How to Find a Fully MIT Version

To use a version of this software under the MIT license with no restrictions, check out any commit
that is older than 2 years:

```bash
git checkout $(git rev-list -n 1 --before="2 years ago" main)
```

If the `LICENSE` file in that commit is FSL-1.1-MIT, the "Grant of Future License" section grants
you MIT rights because the second anniversary has passed.

## Third-Party Code

Some vendored or forked libraries in this repository are licensed under their original licenses
(typically MIT), not FSL-1.1-MIT. These include:

- `libs/elixir-cabbage/` — MIT (Matt Widmann, 2017)
- `libs/elixir-gherkin/` — MIT (Matt Widmann, 2018)
- `archived/ayokoding-web-hugo/` — MIT (Xin, 2023)

Check the `LICENSE` file in each subdirectory for the applicable license.

## More Information

- [Full license text](./LICENSE)
- [FSL official site](https://fsl.software/)
- [FSL FAQ](https://fsl.software/)
