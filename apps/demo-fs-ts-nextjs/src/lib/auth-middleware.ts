import { NextRequest, NextResponse } from "next/server";
import { verifyToken, type JwtClaims } from "./jwt";
import type { SessionRepository } from "@/repositories/interfaces";

export async function requireAuth(req: NextRequest, sessions: SessionRepository): Promise<JwtClaims | NextResponse> {
  const authHeader = req.headers.get("authorization");
  if (!authHeader?.startsWith("Bearer ")) {
    return NextResponse.json({ error: "Missing Authorization header" }, { status: 401 });
  }

  const token = authHeader.slice(7);
  const claims = await verifyToken(token);
  if (!claims) {
    return NextResponse.json({ error: "Invalid or expired token" }, { status: 401 });
  }

  if (claims.tokenType !== "access") {
    return NextResponse.json({ error: "Not an access token" }, { status: 401 });
  }

  const isRevoked = await sessions.isAccessTokenRevoked(claims.jti);
  if (isRevoked) {
    return NextResponse.json({ error: "Token has been revoked" }, { status: 401 });
  }

  return claims;
}

export async function requireAdmin(req: NextRequest, sessions: SessionRepository): Promise<JwtClaims | NextResponse> {
  const result = await requireAuth(req, sessions);
  if (result instanceof NextResponse) return result;

  if (result.role !== "ADMIN") {
    return NextResponse.json({ error: "Admin access required" }, { status: 403 });
  }

  return result;
}

export function serviceResponse<T>(
  result: { ok: true; data: T } | { ok: false; error: string; status: number },
  successStatus = 200,
): NextResponse {
  if (result.ok) {
    return NextResponse.json(result.data, { status: successStatus });
  }
  return NextResponse.json({ error: result.error }, { status: result.status });
}
