defmodule Backend.Clusters do
  @moduledoc """
  The Clusters context.
  """

  import Ecto.Query, warn: false
  alias Backend.Repo

  alias Backend.Clusters.Cluster

  @doc """
  Returns the list of clusters.

  ## Examples

      iex> list_clusters()
      [%Cluster{}, ...]

  """
  def list_clusters(user_id) do
    query =
      from c in Cluster,
        where: c.user_id == ^user_id

    Repo.all(query)
  end
  def list_available_clusters(user_id) do
    query =
      from c in Cluster,
        where: c.user_id == ^user_id and is_nil(c.task_id)

    Repo.all(query)
  end
  @doc """
  Gets a single cluster.

  Raises `Ecto.NoResultsError` if the Cluster does not exist.

  ## Examples

      iex> get_cluster!(123)
      %Cluster{}

      iex> get_cluster!(456)
      ** (Ecto.NoResultsError)

  """
  def get_cluster!(id, user_id) do
    query =
      from c in Cluster,
        where: c.user_id == ^user_id and c.id == ^id

    Repo.one!(query)
  end

  def get_available_cluster!(id, user_id) do
    query =
      from c in Cluster,
        where: c.user_id == ^user_id and c.id == ^id and is_nil(c.task_id)
    Repo.one!(query)
  end
  @doc """
  Creates a cluster.

  ## Examples

      iex> create_cluster(%{field: value})
      {:ok, %Cluster{}}

      iex> create_cluster(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_cluster(attrs \\ %{}) do
    %Cluster{}
    |> Cluster.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a cluster.

  ## Examples

      iex> update_cluster(cluster, %{field: new_value})
      {:ok, %Cluster{}}

      iex> update_cluster(cluster, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_cluster(%Cluster{} = cluster, attrs) do
    cluster
    |> Cluster.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a cluster.

  ## Examples

      iex> delete_cluster(cluster)
      {:ok, %Cluster{}}

      iex> delete_cluster(cluster)
      {:error, %Ecto.Changeset{}}

  """
  def delete_cluster(%Cluster{} = cluster) do
    Repo.delete(cluster)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking cluster changes.

  ## Examples

      iex> change_cluster(cluster)
      %Ecto.Changeset{data: %Cluster{}}

  """
  def change_cluster(%Cluster{} = cluster, attrs \\ %{}) do
    Cluster.changeset(cluster, attrs)
  end
end
