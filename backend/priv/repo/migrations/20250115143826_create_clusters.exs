defmodule Backend.Repo.Migrations.CreateClusters do
  use Ecto.Migration

  def change do
    create table(:clusters) do
      add :name, :string
      add :cluster_cookie, :string
      add :user_id, references(:users, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:clusters, [:user_id])
  end
end
