defmodule ADemoBeExph.UserChangesetTest do
  use ExUnit.Case, async: true

  alias ADemoBeExph.Accounts.User

  @moduletag :unit

  @valid_attrs %{username: "alice", email: "alice@example.com", password: "Str0ng#Pass1"}

  describe "changeset/2 with valid data" do
    test "is valid with proper username, email, and strong password" do
      changeset = User.changeset(%User{}, @valid_attrs)
      assert changeset.valid?
    end

    test "hashes the password when valid" do
      changeset = User.changeset(%User{}, @valid_attrs)
      assert changeset.valid?
      assert get_change(changeset, :password_hash) != nil
      assert get_change(changeset, :password_hash) != @valid_attrs.password
    end
  end

  describe "changeset/2 username validations" do
    test "is invalid with empty username" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, ""))
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is invalid when username is nil" do
      changeset = User.changeset(%User{}, Map.delete(@valid_attrs, :username))
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is invalid when username is too short (2 chars)" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "ab"))
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is valid when username is exactly 3 chars" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "abc"))
      assert changeset.valid?
    end

    test "is invalid with username containing spaces" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "invalid user"))
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is invalid with username containing special characters" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "invalid!"))
      refute changeset.valid?
      assert changeset.errors[:username] != nil
    end

    test "is valid with username containing underscores" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "valid_user"))
      assert changeset.valid?
    end

    test "is valid with username containing digits" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "user123"))
      assert changeset.valid?
    end
  end

  describe "changeset/2 email validations" do
    test "is invalid with empty email" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :email, ""))
      refute changeset.valid?
      assert changeset.errors[:email] != nil
    end

    test "is invalid when email is nil" do
      changeset = User.changeset(%User{}, Map.delete(@valid_attrs, :email))
      refute changeset.valid?
      assert changeset.errors[:email] != nil
    end

    test "is invalid with bad email format" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :email, "not-an-email"))
      refute changeset.valid?
      assert changeset.errors[:email] != nil
    end
  end

  describe "changeset/2 password validations" do
    test "is invalid with empty password" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :password, ""))
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password is nil" do
      changeset = User.changeset(%User{}, Map.delete(@valid_attrs, :password))
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password is too short (less than 12 chars)" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :password, "Short1!Ab"))
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password has no uppercase letter" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :password, "str0ng#pass1x"))
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "is invalid when password has no special character" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :password, "AllUpperCase1234"))
      refute changeset.valid?
      assert changeset.errors[:password] != nil
    end

    test "does not hash password when changeset is invalid" do
      changeset = User.changeset(%User{}, Map.put(@valid_attrs, :username, "ab"))
      refute changeset.valid?
      assert get_change(changeset, :password_hash) == nil
    end
  end

  defp get_change(changeset, key) do
    changeset.changes[key]
  end
end
