defmodule Backend.Repo.Migrations.DeleteNodesTable do
  use Ecto.Migration

  def change do
    drop table("nodes")
  end
end
