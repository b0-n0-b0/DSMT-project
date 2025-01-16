defmodule BackendWeb.BackendWeb.NodeHTML do
  use BackendWeb, :html

  embed_templates "node_html/*"

  @doc """
  Renders a node form.
  """
  attr :changeset, Ecto.Changeset, required: true
  attr :action, :string, required: true

  def node_form(assigns)
end
