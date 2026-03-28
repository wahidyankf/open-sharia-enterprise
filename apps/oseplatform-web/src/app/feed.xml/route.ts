import { serverCaller } from "@/lib/trpc/server";

export const dynamic = "force-static";

export async function GET() {
  const updates = await serverCaller.content.listUpdates();

  const siteUrl = "https://oseplatform.com";
  const items = updates
    .map((update) => {
      const dateStr = update.date ? new Date(update.date).toUTCString() : "";
      return `    <item>
      <title><![CDATA[${update.title}]]></title>
      <link>${siteUrl}/${update.slug}/</link>
      <guid>${siteUrl}/${update.slug}/</guid>
      ${dateStr ? `<pubDate>${dateStr}</pubDate>` : ""}
      ${update.summary ? `<description><![CDATA[${update.summary}]]></description>` : ""}
    </item>`;
    })
    .join("\n");

  const rss = `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>OSE Platform Updates</title>
    <link>${siteUrl}/updates/</link>
    <description>Updates on the Open Sharia Enterprise Platform development</description>
    <language>en</language>
    <atom:link href="${siteUrl}/feed.xml" rel="self" type="application/rss+xml"/>
${items}
  </channel>
</rss>`;

  return new Response(rss, {
    headers: {
      "Content-Type": "application/xml",
    },
  });
}
