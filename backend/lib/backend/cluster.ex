defmodule Backend.Cluster do
  use Ecto.Schema
  import Ecto.Changeset

  schema "clusters" do
    field :name, :string
    field :cluster_cookie, :string
    field :user_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(cluster, attrs) do
    cluster
    |> cast(attrs, [:name, :cluster_cookie])
    |> validate_required([:name, :cluster_cookie])
  end
end
