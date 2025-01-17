defmodule Backend.Repo.Migrations.ClustersAddCurrentTaskColumn do
  use Ecto.Migration

  def change do
    alter table("clusters") do
      add :task_id, references("tasks"), default: nil, null: true
    end
  end
end
