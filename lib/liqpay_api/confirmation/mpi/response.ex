defmodule LiqPayAPI.Confirmation.MPI.Response do
  @moduledoc """
  Provides struct and type for a Confirmation.MPI.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          cres: String.t() | nil,
          mpi_cres: String.t() | nil,
          mpi_form: String.t() | nil,
          mpi_req_md: String.t() | nil,
          mpi_req_pareq: String.t() | nil,
          mpi_req_url: String.t() | nil,
          mpi_status: :a | :c | :n | :u | :y | String.t() | nil,
          mpi_version: :"2/0" | String.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: :failure | :success | String.t() | nil
        }
  @type types :: :t

  defstruct [
    :cres,
    :mpi_cres,
    :mpi_form,
    :mpi_req_md,
    :mpi_req_pareq,
    :mpi_req_url,
    :mpi_status,
    :mpi_version,
    :result,
    :status
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      cres: {"cres", {:string, :generic}},
      mpi_cres: {"mpi_cres", {:string, :generic}},
      mpi_form: {"mpi_form", {:string, :generic}},
      mpi_req_md: {"mpi_req_md", {:string, :generic}},
      mpi_req_pareq: {"mpi_req_pareq", {:string, :generic}},
      mpi_req_url: {"mpi_req_url", {:string, :generic}},
      mpi_status:
        {"mpi_status",
         {:enum, [{:a, "A"}, {:c, "C"}, {:n, "N"}, {:u, "U"}, {:y, "Y"}, :not_strict]}},
      mpi_version: {"mpi_version", {:enum, [{:"2/0", "2.0"}, :not_strict]}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status: {"status", {:enum, [{:failure, "failure"}, {:success, "success"}, :not_strict]}}
    ]
  end
end
