defmodule Backend.Tasks.Task do
  use Ecto.Schema
  import Ecto.Changeset

  schema "tasks" do
    field :description, :string
    field :title, :string
    field :erlang_model, :string
    field :user_id, :id
    field :status, :string #status can be: ready, running, done, failed
    field :controller_url, :string

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(task, attrs) do
    task
    |> cast(attrs, [:title, :description, :erlang_model,:user_id, :status, :controller_url])
    |> validate_required([:title, :description, :erlang_model,:user_id, :status])
  end
end
