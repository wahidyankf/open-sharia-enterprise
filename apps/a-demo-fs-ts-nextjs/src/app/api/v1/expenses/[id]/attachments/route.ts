import { NextRequest, NextResponse } from "next/server";
import { getRepositories } from "@/repositories";
import { uploadAttachment, listAttachments } from "@/services/attachment-service";
import { requireAuth, serviceResponse } from "@/lib/auth-middleware";

type Params = { params: Promise<{ id: string }> };

export async function GET(req: NextRequest, { params }: Params) {
  const repos = await getRepositories();
  const authResult = await requireAuth(req, repos.sessions);
  if (authResult instanceof NextResponse) return authResult;

  const { id } = await params;
  const result = await listAttachments(repos, id, authResult.sub);
  if (!result.ok) return serviceResponse(result);
  return NextResponse.json({ attachments: result.data }, { status: 200 });
}

export async function POST(req: NextRequest, { params }: Params) {
  try {
    const repos = await getRepositories();
    const authResult = await requireAuth(req, repos.sessions);
    if (authResult instanceof NextResponse) return authResult;

    const { id } = await params;

    let formData: FormData;
    try {
      formData = await req.formData();
    } catch {
      return NextResponse.json({ message: "Failed to parse multipart form data" }, { status: 400 });
    }

    const file = formData.get("file") as File | null;

    if (!file) {
      return NextResponse.json({ message: "File is required" }, { status: 400 });
    }

    const arrayBuf = await file.arrayBuffer();
    const buffer = Buffer.from(arrayBuf);
    const result = await uploadAttachment(repos, id, authResult.sub, {
      filename: file.name,
      contentType: file.type,
      size: buffer.length,
      data: buffer,
    });
    if (!result.ok) return serviceResponse(result);
    return NextResponse.json(
      { ...result.data, url: `/api/v1/expenses/${id}/attachments/${result.data.id}` },
      { status: 201 },
    );
  } catch (err) {
    console.error("[POST /attachments] Unhandled error:", err);
    return NextResponse.json({ message: "Internal server error" }, { status: 500 });
  }
}
