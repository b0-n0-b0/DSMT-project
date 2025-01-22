defmodule Backend.Repo.Migrations.NewClusterTable do
  use Ecto.Migration

  def change do
    alter table("clusters") do
      add :cluster_controller_url, :string
      add :cluster_api_key, :string
      remove :cluster_cookie
    end
  end
end
