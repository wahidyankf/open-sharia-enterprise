defmodule OrganicleverBeExph.UserChangesetTest do
  use ExUnit.Case, async: true

  alias OrganicleverBeExph.Accounts.User

  @moduletag :unit

  describe "changeset/2 with valid data" do
    test "is valid with proper username and strong password" do
      changeset = User.changeset(%User{}, %{username: "alice", password: "s3cur3Pass!"})
      assert changeset.valid?
    end

    test "hashes the password when valid" do
      changeset = User.changeset(%User{}, %{username: "alice", password: "s3cur3Pass!"})
      assert changeset.valid?
      assert get_change(changeset, :password_hash) != nil
      assert get_change(changeset, :password_hash) != "s3cur3Pass!"
    end
  end

  describe "changeset/2 username validations" do
    test "is invalid with empty username" do
      changeset = User.changeset(%User{}, %{username: "", password: "s3cur3Pass!"})
      refute changeset.valid?
      assert errors = changeset.errors[:username]
      assert errors != nil
    end

    test "is invalid when username is nil" do
      changeset = User.changeset(%User{}, %{password: "s3cur3Pass!"})
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is invalid when username is too short (2 chars)" do
      changeset = User.changeset(%User{}, %{username: "ab", password: "s3cur3Pass!"})
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is valid when username is exactly 3 chars" do
      changeset = User.changeset(%User{}, %{username: "abc", password: "s3cur3Pass!"})
      assert changeset.valid?
    end

    test "is invalid with username containing spaces" do
      changeset = User.changeset(%User{}, %{username: "invalid user", password: "s3cur3Pass!"})
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is invalid with username containing special characters" do
      changeset = User.changeset(%User{}, %{username: "invalid!", password: "s3cur3Pass!"})
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is valid with username containing underscores" do
      changeset = User.changeset(%User{}, %{username: "valid_user", password: "s3cur3Pass!"})
      assert changeset.valid?
    end

    test "is valid with username containing digits" do
      changeset = User.changeset(%User{}, %{username: "user123", password: "s3cur3Pass!"})
      assert changeset.valid?
    end
  end

  describe "changeset/2 password validations" do
    test "is invalid with empty password" do
      changeset = User.changeset(%User{}, %{username: "validuser", password: ""})
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password is nil" do
      changeset = User.changeset(%User{}, %{username: "validuser"})
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password is too short (less than 8 chars)" do
      changeset = User.changeset(%User{}, %{username: "validuser", password: "Short1!"})
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password has no uppercase letter" do
      changeset = User.changeset(%User{}, %{username: "validuser", password: "alllower1!"})
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password has no special character" do
      changeset = User.changeset(%User{}, %{username: "validuser", password: "NoSpecial1"})
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "does not hash password when changeset is invalid" do
      changeset = User.changeset(%User{}, %{username: "ab", password: "s3cur3Pass!"})
      refute changeset.valid?
      assert get_change(changeset, :password_hash) == nil
    end
  end

  defp get_change(changeset, key) do
    changeset.changes[key]
  end
end
