defmodule Backend.Clusters.Cluster do
  use Ecto.Schema
  import Ecto.Changeset

  schema "clusters" do
    field :name, :string
    field :cluster_api_key, :string
    field :cluster_controller_url, :string
    field :user_id, :id
    field :task_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(cluster, attrs) do
    cluster
    |> cast(attrs, [:name, :cluster_api_key, :cluster_controller_url, :user_id, :task_id])
    |> validate_required([:name, :cluster_api_key, :cluster_controller_url, :user_id])
  end
end
