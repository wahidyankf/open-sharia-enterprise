import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";
import { uploadAttachment, listAttachments } from "@/services/attachment-service";
import { requireAuth, serviceResponse } from "@/lib/auth-middleware";
import { MAX_ATTACHMENT_SIZE } from "@/lib/types";

type Params = { params: Promise<{ id: string }> };

export async function GET(req: NextRequest, { params }: Params) {
  const repos = getRepositories();
  const authResult = await requireAuth(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const { id } = await params;
  const result = await listAttachments(repos, id, authResult.sub);
  return serviceResponse(result);
}

export async function POST(req: NextRequest, { params }: Params) {
  const repos = getRepositories();
  const authResult = await requireAuth(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const { id } = await params;
  const formData = await req.formData();
  const file = formData.get("file") as File | null;

  if (!file) {
    return NextResponse.json({ error: "File is required" }, { status: 400 });
  }

  if (file.size > MAX_ATTACHMENT_SIZE) {
    return NextResponse.json(
      { error: `File size exceeds maximum of ${MAX_ATTACHMENT_SIZE / (1024 * 1024)}MB` },
      { status: 400 },
    );
  }

  const buffer = Buffer.from(await file.arrayBuffer());
  const result = await uploadAttachment(repos, id, authResult.sub, {
    filename: file.name,
    contentType: file.type,
    size: file.size,
    data: buffer,
  });
  return serviceResponse(result, 201);
}
