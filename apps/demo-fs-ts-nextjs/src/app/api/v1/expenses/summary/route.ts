import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";
import { getExpenseSummary } from "@/services/expense-service";
import { requireAuth, serviceResponse } from "@/lib/auth-middleware";

export async function GET(req: NextRequest) {
  const repos = getRepositories();
  const authResult = await requireAuth(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const result = await getExpenseSummary(repos, authResult.sub);
  return serviceResponse(result);
}
