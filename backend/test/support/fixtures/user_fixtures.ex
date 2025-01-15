defmodule Backend.UserFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Backend.User` context.
  """

  @doc """
  Generate a task.
  """
  def task_fixture(attrs \\ %{}) do
    {:ok, task} =
      attrs
      |> Enum.into(%{
        description: "some description",
        erlang_model: "some erlang_model",
        title: "some title"
      })
      |> Backend.User.create_task()

    task
  end
end
