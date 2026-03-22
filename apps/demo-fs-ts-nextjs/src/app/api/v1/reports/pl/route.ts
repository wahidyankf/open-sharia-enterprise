import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";
import { generatePLReport } from "@/services/report-service";
import { requireAuth, serviceResponse } from "@/lib/auth-middleware";

export async function GET(req: NextRequest) {
  const repos = getRepositories();
  const authResult = await requireAuth(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const url = new URL(req.url);
  const startDate = url.searchParams.get("startDate") ?? undefined;
  const endDate = url.searchParams.get("endDate") ?? undefined;

  const result = await generatePLReport(repos, authResult.sub, startDate, endDate);
  return serviceResponse(result);
}
