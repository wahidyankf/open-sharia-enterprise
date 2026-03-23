import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";

export async function POST(req: NextRequest) {
  if (process.env.ENABLE_TEST_API !== "true") {
    return NextResponse.json({ message: "Test API is disabled" }, { status: 404 });
  }

  const body = await req.json();
  const username = body.username as string;
  if (!username) {
    return NextResponse.json({ message: "Username is required" }, { status: 400 });
  }

  const repos = await getRepositories();
  const user = await repos.users.findByUsername(username);
  if (!user) {
    return NextResponse.json({ message: "User not found" }, { status: 404 });
  }

  await repos.users.updateRole(user.id, "ADMIN");
  return NextResponse.json({ message: `User ${username} promoted to ADMIN` });
}
