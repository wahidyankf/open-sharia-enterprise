# wahidyankf-web — Product Overview

**Audience:** Engineers, Technical Product/Project Managers

`wahidyankf-web` is a personal portfolio and CV site for Wahidyan Kresna Fridayoka,
deployed at [www.wahidyankf.com](https://www.wahidyankf.com).

## Personas

| Persona             | Goal                                                          |
| ------------------- | ------------------------------------------------------------- |
| Technical recruiter | Quickly assess technical background and contact the candidate |
| Hiring manager      | Evaluate career progression and engineering depth             |
| Collaborator        | Find project links and contact channels                       |

## Primary user flows

1. **Landing** — visitor arrives at `/`, scans top skills and about-me summary
2. **CV deep-dive** — visitor navigates to `/cv`, browses work history timeline
3. **Projects browse** — visitor navigates to `/personal-projects`, filters by technology
4. **Search** — visitor uses the search box to find specific skills, languages, or entries
5. **Contact** — visitor clicks a social/contact link from home or CV page

## Current scope

- Home page with top skills, languages, frameworks, and about-me
- CV page with full work history timeline
- Personal projects page with tech-tag filter
- Client-side search across all content
- Dark mode toggle via theme-toggle from `@open-sharia-enterprise/web-ui`
- Responsive navigation (sidebar on desktop, bottom tab bar on mobile)

## Out of scope (not planned)

- Authentication or personalization
- Server-side data fetching or API
- Comments or contact form (social links only)
