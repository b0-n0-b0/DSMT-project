defmodule Backend.Repo.Migrations.CreateNodes do
  use Ecto.Migration

  def change do
    create table(:nodes) do
      add :name, :string
      add :status, :integer
      add :cluster_id, references(:clusters, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:nodes, [:cluster_id])
  end
end
