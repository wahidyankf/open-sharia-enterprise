import { NextResponse } from "next/server";
import { serialize } from "cookie";

export async function POST() {
  // Clear the auth cookie by setting it to an empty value and expiring it
  const cookie = serialize("auth_token", "", {
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
    sameSite: "strict",
    maxAge: -1, // Expire the cookie immediately
    path: "/",
  });

  return NextResponse.json(
    { success: true },
    {
      status: 200,
      headers: { "Set-Cookie": cookie },
    },
  );
}
