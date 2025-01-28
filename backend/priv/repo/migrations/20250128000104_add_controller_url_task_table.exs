defmodule Backend.Repo.Migrations.AddControllerUrlTaskTable do
  use Ecto.Migration

  def change do
    alter table("tasks") do
      add :controller_url, :text
    end  end
end
