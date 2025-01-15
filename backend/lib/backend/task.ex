defmodule Backend.Task do
  use Ecto.Schema
  import Ecto.Changeset

  schema "tasks" do
    field :description, :string
    field :title, :string
    field :erlang_model, :string
    field :user_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(task, attrs) do
    task
    |> cast(attrs, [:title, :description, :erlang_model])
    |> validate_required([:title, :description, :erlang_model])
  end
end
