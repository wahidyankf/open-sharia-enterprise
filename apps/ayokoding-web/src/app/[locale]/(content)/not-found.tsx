import Link from "next/link";
import { Button } from "@open-sharia-enterprise/ts-ui";

export default function ContentNotFound() {
  return (
    <div className="flex flex-1 flex-col items-center justify-center px-4 py-16">
      <h2 className="mb-4 text-2xl font-bold">Page Not Found</h2>
      <p className="mb-6 text-muted-foreground">The page you are looking for does not exist or has been moved.</p>
      <Button asChild>
        <Link href="/en">Go Home</Link>
      </Button>
    </div>
  );
}
