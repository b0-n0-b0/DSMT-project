defmodule Backend.Node do
  use Ecto.Schema
  import Ecto.Changeset

  schema "nodes" do
    field :name, :string
    field :status, :integer
    field :cluster_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(node, attrs) do
    node
    |> cast(attrs, [:name, :status])
    |> validate_required([:name, :status])
  end
end
