import { buildFeed } from "@/contexts/rss-feed/application/feed-builder";

export const dynamic = "force-static";

export const GET = buildFeed;
