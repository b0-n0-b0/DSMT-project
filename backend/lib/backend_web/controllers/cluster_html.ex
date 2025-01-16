defmodule BackendWeb.ClusterHTML do
  use BackendWeb, :html

  embed_templates "cluster_html/*"

  @doc """
  Renders a cluster form.
  """
  attr :changeset, Ecto.Changeset, required: true
  attr :action, :string, required: true

  def cluster_form(assigns)
end
