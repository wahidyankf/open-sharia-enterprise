"use client";

import { useState } from "react";
import {
  useAdminUsers,
  useDisableUser,
  useEnableUser,
  useUnlockUser,
  useForcePasswordReset,
} from "@/lib/queries/use-admin";
import type { User } from "@/lib/api/types";

const btnCn = (colorCn: string) =>
  `px-2.5 py-1.5 ${colorCn} text-white border-none rounded cursor-pointer text-xs font-semibold mr-1.5`;

export default function AdminPage() {
  const [page, setPage] = useState(0);
  const [searchInput, setSearchInput] = useState("");
  const [search, setSearch] = useState<string | undefined>(undefined);
  const [generatedToken, setGeneratedToken] = useState<{ userId: string; token: string; copied: boolean } | null>(null);
  const [disableReason, setDisableReason] = useState("");
  const [disablingUserId, setDisablingUserId] = useState<string | null>(null);

  const { data, isLoading, isError } = useAdminUsers(page, 20, search);
  const disableMutation = useDisableUser();
  const enableMutation = useEnableUser();
  const unlockMutation = useUnlockUser();
  const resetMutation = useForcePasswordReset();

  const handleSearch = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setSearch(searchInput.trim() || undefined);
    setPage(0);
  };

  const handleDisable = (userId: string) => {
    if (!disableReason.trim()) return;
    disableMutation.mutate(
      { userId, data: { reason: disableReason } },
      {
        onSuccess: () => {
          setDisablingUserId(null);
          setDisableReason("");
        },
      },
    );
  };

  const handleCopyToken = (userId: string) => {
    resetMutation.mutate(userId, {
      onSuccess: (result) => {
        setGeneratedToken({ userId, token: result.token, copied: false });
      },
    });
  };

  const handleCopyToClipboard = () => {
    if (!generatedToken) return;
    void navigator.clipboard.writeText(generatedToken.token).then(() => {
      setGeneratedToken((prev) => (prev ? { ...prev, copied: true } : null));
      setTimeout(() => setGeneratedToken((prev) => (prev ? { ...prev, copied: false } : null)), 3000);
    });
  };

  const totalPages = data?.totalPages ?? 1;

  const statusBadge = (status: string) => {
    const colorMap: Record<string, string> = {
      ACTIVE: "bg-green-600",
      INACTIVE: "bg-orange-500",
      DISABLED: "bg-red-700",
      LOCKED: "bg-purple-700",
    };
    return (
      <span className={`${colorMap[status] ?? "bg-gray-500"} rounded px-2 py-0.5 text-xs font-semibold text-white`}>
        {status}
      </span>
    );
  };

  const renderActions = (user: User) => (
    <td className="px-3 py-3 whitespace-nowrap">
      {user.status === "ACTIVE" && (
        <button
          className={btnCn("bg-red-700")}
          onClick={() => setDisablingUserId(user.id)}
          disabled={disableMutation.isPending}
          aria-label={`Disable user ${user.username}`}
        >
          Disable
        </button>
      )}
      {user.status === "DISABLED" && (
        <button
          className={btnCn("bg-green-600")}
          onClick={() => enableMutation.mutate(user.id)}
          disabled={enableMutation.isPending}
          aria-label={`Enable user ${user.username}`}
        >
          Enable
        </button>
      )}
      {user.status === "LOCKED" && (
        <button
          className={btnCn("bg-purple-700")}
          onClick={() => unlockMutation.mutate(user.id)}
          disabled={unlockMutation.isPending}
          aria-label={`Unlock user ${user.username}`}
        >
          Unlock
        </button>
      )}
      <button
        className={btnCn("bg-blue-600")}
        onClick={() => handleCopyToken(user.id)}
        disabled={resetMutation.isPending}
        aria-label={`Generate Reset Token for ${user.username}`}
      >
        Generate Reset Token
      </button>
      {generatedToken?.userId === user.id && (
        <span className="mt-1.5 inline-flex items-center gap-1.5">
          <code data-testid="reset-token" className="rounded bg-gray-100 px-1.5 py-0.5 text-xs break-all">
            {generatedToken.token}
          </code>
          <button
            className={btnCn(generatedToken.copied ? "bg-green-600" : "bg-gray-600")}
            onClick={handleCopyToClipboard}
            aria-label="Copy token"
          >
            {generatedToken.copied ? "Copied!" : "Copy"}
          </button>
        </span>
      )}
    </td>
  );

  return (
    <>
      <h1 className="mb-6">Admin: Users</h1>

      <form onSubmit={handleSearch} className="mb-6 flex gap-3">
        <label htmlFor="search-users" className="hidden">
          Search users
        </label>
        <input
          id="search-users"
          type="text"
          value={searchInput}
          onChange={(e) => setSearchInput(e.target.value)}
          placeholder="Search by username or email"
          className="flex-1 rounded border border-gray-400 px-3 py-2.5 text-base"
        />
        <button
          type="submit"
          className="cursor-pointer rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white"
        >
          Search
        </button>
        {search && (
          <button
            type="button"
            onClick={() => {
              setSearch(undefined);
              setSearchInput("");
            }}
            className="cursor-pointer rounded border border-gray-400 bg-white px-4 py-2.5 text-gray-800"
          >
            Clear
          </button>
        )}
      </form>

      {disablingUserId && (
        <div
          role="alertdialog"
          aria-modal="true"
          aria-labelledby="disable-dialog-title"
          className="fixed inset-0 z-[300] flex items-center justify-center bg-black/40"
        >
          <div className="w-[24rem] rounded-lg bg-white p-6 shadow-2xl">
            <h2 id="disable-dialog-title" className="mt-0">
              Disable User
            </h2>
            <div className="mb-4">
              <label htmlFor="disable-reason" className="mb-1.5 block font-semibold">
                Reason
              </label>
              <textarea
                id="disable-reason"
                value={disableReason}
                onChange={(e) => setDisableReason(e.target.value)}
                rows={3}
                className="box-border w-full resize-y rounded border border-gray-400 px-2.5 py-2.5 text-base"
              />
            </div>
            <div className="flex gap-3">
              <button
                onClick={() => handleDisable(disablingUserId)}
                disabled={disableMutation.isPending || !disableReason.trim()}
                className={btnCn("bg-red-700")}
              >
                {disableMutation.isPending ? "Disabling..." : "Disable"}
              </button>
              <button
                onClick={() => {
                  setDisablingUserId(null);
                  setDisableReason("");
                }}
                className="cursor-pointer rounded border border-gray-400 bg-white px-2.5 py-1.5 text-gray-800"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {isLoading && <p>Loading users...</p>}
      {isError && (
        <p role="alert" className="text-red-700">
          Failed to load users.
        </p>
      )}

      {data && (
        <>
          <p className="mb-3 text-sm text-gray-500">{data.totalElements} users</p>
          <div className="overflow-x-auto">
            <table className="w-full border-collapse overflow-hidden rounded-lg bg-white shadow-md">
              <thead>
                <tr className="bg-gray-200">
                  {["Username", "Email", "Status", "Actions"].map((h) => (
                    <th
                      key={h}
                      className="px-3 py-3 text-left text-sm font-bold tracking-[0.04em] text-gray-600 uppercase"
                    >
                      {h}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {data.content.map((user, idx) => (
                  <tr key={user.id} className={`border-b border-gray-200 ${idx % 2 === 0 ? "bg-white" : "bg-gray-50"}`}>
                    <td className="px-3 py-3">{user.username}</td>
                    <td className="px-3 py-3">{user.email}</td>
                    <td className="px-3 py-3">{statusBadge(user.status)}</td>
                    {renderActions(user)}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          <div className="mt-6 flex items-center justify-center gap-2">
            <button
              onClick={() => setPage((p) => Math.max(0, p - 1))}
              disabled={page === 0}
              aria-label="Previous page"
              className={`rounded border border-gray-400 px-4 py-2 ${page === 0 ? "cursor-not-allowed bg-gray-100" : "cursor-pointer bg-white"}`}
            >
              Previous
            </button>
            <span className="text-gray-600">
              Page {page + 1} of {totalPages}
            </span>
            <button
              onClick={() => setPage((p) => Math.min(totalPages - 1, p + 1))}
              disabled={page >= totalPages - 1}
              aria-label="Next page"
              className={`rounded border border-gray-400 px-4 py-2 ${page >= totalPages - 1 ? "cursor-not-allowed bg-gray-100" : "cursor-pointer bg-white"}`}
            >
              Next
            </button>
          </div>
        </>
      )}
    </>
  );
}
