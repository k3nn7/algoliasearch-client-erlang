-module(algolia_mock_client).
-compile(export_all).

-include("client.hrl").

make(ExpectedRequest, ApiResponse) ->
  #algolia_client{
    app_id = "foo",
    api_key = "bar",
    read_transport = make_read_transport(ExpectedRequest, ApiResponse)
  }.

make_read_transport(ExpectedRequest, Response) ->
  fun (Request) ->
    case Request of
      ExpectedRequest ->
        Response;
      _ ->
        {ok, 404, [], <<"Not found">>}
    end
  end.
