import Link from "next/link";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import type { ContentMeta } from "@/server/content/types";

interface UpdateCardProps {
  update: ContentMeta;
}

export function UpdateCard({ update }: UpdateCardProps) {
  const dateStr = update.date
    ? new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "long",
        day: "numeric",
      }).format(update.date)
    : "";

  return (
    <Card className="transition-colors hover:border-primary/50">
      <Link href={`/${update.slug}/`}>
        <CardHeader>
          <CardTitle className="text-lg">{update.title}</CardTitle>
          {dateStr && (
            <p className="font-mono text-xs text-muted-foreground">{dateStr}</p>
          )}
        </CardHeader>
        <CardContent>
          {update.summary && (
            <p className="line-clamp-3 text-sm text-muted-foreground">
              {update.summary}
            </p>
          )}
          {update.tags.length > 0 && (
            <div className="mt-3 flex flex-wrap gap-1">
              {update.tags.map((tag) => (
                <Badge key={tag} variant="secondary" className="text-xs">
                  {tag}
                </Badge>
              ))}
            </div>
          )}
        </CardContent>
      </Link>
    </Card>
  );
}
