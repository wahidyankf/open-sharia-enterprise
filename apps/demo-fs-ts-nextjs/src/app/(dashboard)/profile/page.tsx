"use client";

import { useState, useEffect } from "react";
import { useRouter } from "next/navigation";
import { useCurrentUser, useUpdateProfile, useChangePassword, useDeactivateAccount } from "@/lib/queries/use-user";
import { useAuth } from "@/lib/auth/auth-provider";
import { ApiError } from "@/lib/api/client";

const inputCn = "w-full px-3 py-2.5 border border-gray-400 rounded text-base box-border";
const labelCn = "block mb-1.5 font-semibold";
const cardCn = "bg-white p-6 rounded-lg border border-gray-300 shadow-md mb-6";

export default function ProfilePage() {
  const router = useRouter();
  const { logout } = useAuth();
  const { data: user, isLoading } = useCurrentUser();
  const updateProfileMutation = useUpdateProfile();
  const changePasswordMutation = useChangePassword();
  const deactivateMutation = useDeactivateAccount();

  const [displayName, setDisplayName] = useState("");
  const [profileSuccess, setProfileSuccess] = useState<string | null>(null);
  const [profileError, setProfileError] = useState<string | null>(null);

  const [oldPassword, setOldPassword] = useState("");
  const [newPassword, setNewPassword] = useState("");
  const [pwSuccess, setPwSuccess] = useState<string | null>(null);
  const [pwError, setPwError] = useState<string | null>(null);

  const [showDeactivateConfirm, setShowDeactivateConfirm] = useState(false);

  useEffect(() => {
    if (user) setDisplayName(user.displayName);
  }, [user]);

  const handleUpdateProfile = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setProfileError(null);
    setProfileSuccess(null);
    updateProfileMutation.mutate(
      { displayName },
      {
        onSuccess: () => setProfileSuccess("Profile updated successfully."),
        onError: () => setProfileError("Failed to update profile."),
      },
    );
  };

  const handleChangePassword = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setPwError(null);
    setPwSuccess(null);
    if (!oldPassword || !newPassword) {
      setPwError("Both fields are required.");
      return;
    }
    changePasswordMutation.mutate(
      { oldPassword, newPassword },
      {
        onSuccess: () => {
          setPwSuccess("Password changed successfully.");
          setOldPassword("");
          setNewPassword("");
        },
        onError: (err) => {
          if (err instanceof ApiError && err.status === 400) {
            setPwError("Current password is incorrect.");
          } else {
            setPwError("Failed to change password.");
          }
        },
      },
    );
  };

  const handleDeactivate = () => {
    deactivateMutation.mutate(undefined, {
      onSuccess: () => {
        logout();
        router.push("/login");
      },
      onError: () => {
        setShowDeactivateConfirm(false);
      },
    });
  };

  if (isLoading) {
    return <p>Loading profile...</p>;
  }

  return (
    <>
      <h1 className="mb-6">Profile</h1>

      <div className={cardCn}>
        <h2 className="mt-0">Account Information</h2>
        <dl className="m-0">
          {[
            ["Username", user?.username],
            ["Email", user?.email],
            ["Display Name", user?.displayName],
            ["Status", user?.status],
          ].map(([label, value]) => (
            <div key={label} className="mb-2 flex gap-4">
              <dt className="min-w-[8rem] font-semibold">{label}</dt>
              <dd className="m-0 text-gray-600">{value}</dd>
            </div>
          ))}
        </dl>
      </div>

      <div className={cardCn}>
        <h2 className="mt-0">Edit Display Name</h2>

        {profileSuccess && (
          <div role="status" className="mb-4 rounded bg-green-50 px-4 py-2.5 text-green-700">
            {profileSuccess}
          </div>
        )}
        {profileError && (
          <div id="profile-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-2.5 text-red-700">
            {profileError}
          </div>
        )}

        <form onSubmit={handleUpdateProfile} aria-describedby={profileError ? "profile-error" : undefined}>
          <div className="mb-4">
            <label htmlFor="displayName" className={labelCn}>
              Display Name
            </label>
            <input
              id="displayName"
              type="text"
              value={displayName}
              onChange={(e) => setDisplayName(e.target.value)}
              className={inputCn}
            />
          </div>
          <button
            type="submit"
            disabled={updateProfileMutation.isPending}
            className={`rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white ${updateProfileMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
          >
            {updateProfileMutation.isPending ? "Saving..." : "Save Changes"}
          </button>
        </form>
      </div>

      <div className={cardCn}>
        <h2 className="mt-0">Change Password</h2>

        {pwSuccess && (
          <div role="status" className="mb-4 rounded bg-green-50 px-4 py-2.5 text-green-700">
            {pwSuccess}
          </div>
        )}
        {pwError && (
          <div id="pw-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-2.5 text-red-700">
            {pwError}
          </div>
        )}

        <form onSubmit={handleChangePassword} aria-describedby={pwError ? "pw-error" : undefined}>
          <div className="mb-4">
            <label htmlFor="oldPassword" className={labelCn}>
              Current Password
            </label>
            <input
              id="oldPassword"
              type="password"
              value={oldPassword}
              onChange={(e) => setOldPassword(e.target.value)}
              autoComplete="current-password"
              className={inputCn}
            />
          </div>
          <div className="mb-4">
            <label htmlFor="newPassword" className={labelCn}>
              New Password
            </label>
            <input
              id="newPassword"
              type="password"
              value={newPassword}
              onChange={(e) => setNewPassword(e.target.value)}
              autoComplete="new-password"
              className={inputCn}
            />
          </div>
          <button
            type="submit"
            disabled={changePasswordMutation.isPending}
            className={`rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white ${changePasswordMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
          >
            {changePasswordMutation.isPending ? "Changing..." : "Change Password"}
          </button>
        </form>
      </div>

      <div className={cardCn}>
        <h2 className="mt-0 text-red-700">Danger Zone</h2>

        {!showDeactivateConfirm ? (
          <button
            onClick={() => setShowDeactivateConfirm(true)}
            className="cursor-pointer rounded border-none bg-red-700 px-5 py-2.5 font-semibold text-white"
          >
            Deactivate Account
          </button>
        ) : (
          <div
            role="alertdialog"
            aria-modal="true"
            aria-labelledby="deactivate-dialog-title"
            className="rounded border border-red-300 bg-red-50 p-4"
          >
            <p id="deactivate-dialog-title" className="mt-0 font-semibold">
              Are you sure you want to deactivate your account?
            </p>
            <p className="text-sm text-gray-500">This action cannot be undone. You will be logged out immediately.</p>
            <div className="flex gap-3">
              <button
                onClick={handleDeactivate}
                disabled={deactivateMutation.isPending}
                className={`rounded border-none bg-red-700 px-5 py-2.5 font-semibold text-white ${deactivateMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
              >
                {deactivateMutation.isPending ? "Deactivating..." : "Yes, Deactivate"}
              </button>
              <button
                onClick={() => setShowDeactivateConfirm(false)}
                className="cursor-pointer rounded border border-gray-400 bg-white px-5 py-2.5 text-gray-800"
              >
                Cancel
              </button>
            </div>
          </div>
        )}
      </div>
    </>
  );
}
