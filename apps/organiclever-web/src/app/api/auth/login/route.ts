import { NextResponse } from "next/server";
import fs from "fs";
import path from "path";
import { serialize } from "cookie";

export async function POST(request: Request) {
  const { email, password } = await request.json();

  const usersFilePath = path.join(process.cwd(), "src", "data", "users.json");
  const usersData = JSON.parse(fs.readFileSync(usersFilePath, "utf8"));

  const user = usersData.find((u: { email: string; password: string }) => u.email === email && u.password === password);

  if (user) {
    // Create a session token (in a real app, use a more secure method)
    const token = Buffer.from(email).toString("base64");

    // Set the cookie
    const cookie = serialize("auth_token", token, {
      httpOnly: true,
      secure: process.env.NODE_ENV === "production",
      sameSite: "strict",
      maxAge: 3600, // 1 hour
      path: "/",
    });

    return NextResponse.json(
      { success: true },
      {
        status: 200,
        headers: { "Set-Cookie": cookie },
      },
    );
  } else {
    return NextResponse.json({ success: false, message: "Invalid credentials" }, { status: 401 });
  }
}
