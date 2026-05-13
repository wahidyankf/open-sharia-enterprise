// Generic storage-layer error types shared across bounded contexts.
//
// `StorageUnavailable` represents a failure to reach the underlying store.
//
// `NotFound` is the cross-context "entity not found by id" error raised
// by every storage-backed read/update/delete path. Both are platform-shaped,
// not aggregate-shaped, so they live in `shared/` rather than any single
// context's `domain/`.

import { Data } from "effect";

export class StorageUnavailable extends Data.TaggedError("StorageUnavailable")<{
  readonly cause: unknown;
}> {}

export class NotFound extends Data.TaggedError("NotFound")<{
  readonly id: string;
}> {}
