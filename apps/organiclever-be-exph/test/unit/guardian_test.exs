defmodule OrganicleverBeExph.Auth.GuardianTest do
  use ExUnit.Case, async: true

  alias OrganicleverBeExph.Auth.Guardian

  @moduletag :unit

  describe "subject_for_token/2" do
    test "returns ok tuple with stringified id for user map" do
      assert {:ok, "42"} = Guardian.subject_for_token(%{id: 42}, %{})
    end

    test "returns error tuple for unknown resource type" do
      assert {:error, :unknown_resource} = Guardian.subject_for_token("not_a_map", %{})
    end
  end

  describe "resource_from_claims/1" do
    test "returns error tuple for claims without sub key" do
      assert {:error, :missing_subject} = Guardian.resource_from_claims(%{})
    end
  end
end
