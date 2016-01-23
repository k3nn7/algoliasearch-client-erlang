-module(algolia_mock_client).
-compile(export_all).

-include("client.hrl").

make(ExpectedRequest, ApiResponse) ->
  #algolia_client{
    app_id = "foo",
    api_key = "bar",
    transport = make_transport(ExpectedRequest, ApiResponse)
  }.

make_transport(ExpectedRequest, Response) ->
  fun (Request) ->
    case Request of
      ExpectedRequest ->
        Response;
      _ ->
        {error, lists:flatten(io_lib:format("Invalid request: ~p", [Request]))}
    end
  end.
