import { SignJWT, jwtVerify, exportJWK, generateSecret } from "jose";
import type { Role } from "./types";

export interface JwtClaims {
  readonly sub: string;
  readonly username: string;
  readonly role: Role;
  readonly jti: string;
  readonly tokenType: "access" | "refresh";
  readonly iat: number;
  readonly exp: number;
  readonly iss: string;
}

const getSecret = () => {
  const secret = process.env.APP_JWT_SECRET;
  if (!secret) throw new Error("APP_JWT_SECRET is not set");
  return new TextEncoder().encode(secret);
};

const ISSUER = "demo-fs-ts-nextjs";

export async function signAccessToken(userId: string, username: string, role: Role): Promise<string> {
  const jti = crypto.randomUUID();
  return new SignJWT({ username, role, jti, tokenType: "access" })
    .setProtectedHeader({ alg: "HS256" })
    .setSubject(userId)
    .setIssuer(ISSUER)
    .setIssuedAt()
    .setExpirationTime("15m")
    .sign(getSecret());
}

export async function signRefreshToken(userId: string): Promise<string> {
  const jti = crypto.randomUUID();
  return new SignJWT({ jti, tokenType: "refresh" })
    .setProtectedHeader({ alg: "HS256" })
    .setSubject(userId)
    .setIssuer(ISSUER)
    .setIssuedAt()
    .setExpirationTime("7d")
    .sign(getSecret());
}

export async function verifyToken(token: string): Promise<JwtClaims | null> {
  try {
    const { payload } = await jwtVerify(token, getSecret());
    return {
      sub: payload.sub as string,
      username: (payload.username ?? "") as string,
      role: (payload.role ?? "USER") as Role,
      jti: (payload.jti ?? "") as string,
      tokenType: (payload.tokenType ?? "access") as "access" | "refresh",
      iat: (payload.iat ?? 0) as number,
      exp: (payload.exp ?? 0) as number,
      iss: (payload.iss ?? "") as string,
    };
  } catch {
    return null;
  }
}

let cachedJwks: object | null = null;

export async function getJwks(): Promise<object> {
  if (cachedJwks) return cachedJwks;
  const key = await generateSecret("HS256");
  const jwk = await exportJWK(key);
  cachedJwks = { keys: [{ ...jwk, use: "sig", alg: "HS256" }] };
  return cachedJwks;
}
