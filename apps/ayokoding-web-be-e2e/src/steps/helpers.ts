export function buildTrpcUrl(procedure: string, input: unknown): string {
  const encoded = encodeURIComponent(JSON.stringify({ "0": { json: input } }));
  return `/api/trpc/${procedure}?batch=1&input=${encoded}`;
}

export function extractTrpcData(body: unknown): unknown {
  const arr = body as { result: { data: { json: unknown } } }[];
  return arr[0]?.result?.data?.json;
}
