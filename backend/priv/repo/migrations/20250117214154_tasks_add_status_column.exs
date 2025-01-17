defmodule Backend.Repo.Migrations.TasksAddStatusColumn do
  use Ecto.Migration

  def change do
    alter table("tasks") do
      add :status, :string
    end
  end
end
