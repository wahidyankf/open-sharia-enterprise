import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";
import { listUsers } from "@/services/user-service";
import { requireAdmin, serviceResponse } from "@/lib/auth-middleware";

export async function GET(req: NextRequest) {
  const repos = getRepositories();
  const authResult = await requireAdmin(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const url = new URL(req.url);
  const page = parseInt(url.searchParams.get("page") ?? "1", 10);
  const size = parseInt(url.searchParams.get("size") ?? "20", 10);
  const email = url.searchParams.get("email") ?? undefined;

  const result = await listUsers(repos, page, size, email);
  return serviceResponse(result);
}
