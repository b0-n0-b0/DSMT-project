defmodule Backend.Repo.Migrations.FromStringToTextModuleTaskTable do
  use Ecto.Migration

  def change do
    alter table("tasks") do
      remove :erlang_model
      add :erlang_model, :text
    end
  end
end
