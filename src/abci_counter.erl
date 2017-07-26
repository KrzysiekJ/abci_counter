-module(abci_counter).
-behaviour(gen_server).
-behaviour(abci_app).

%% abci_app
-export([handle_request/1]).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(
   state,
   {chain_height=0,
    chain_state=0}).

-include_lib("abci_server/include/abci.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    {Reply, NewState} = handle_request(Request, State),
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% abci_app

handle_request(Request) ->
    gen_server:call(?MODULE, Request).

%% Internal functions.

-spec encode_chain_state(non_neg_integer()) -> binary().
encode_chain_state(0) ->
    %% Special case to make it compliant with ABCI’s implementation and allow not changing genesis.json after running `tendermint init'.
    <<>>;
encode_chain_state(Counter) ->
    binary:encode_unsigned(Counter).

handle_request(#'RequestInfo'{}, State=#state{chain_height=Height, chain_state=Counter}) ->
    {#'ResponseInfo'{
        version="0.1.0",
        last_block_height=Height,
        last_block_app_hash=encode_chain_state(Counter)},
     State};
handle_request(#'RequestBeginBlock'{}, State) ->
    {#'ResponseBeginBlock'{},
     State};
handle_request(#'RequestEndBlock'{}, State) ->
    {#'ResponseEndBlock'{diffs=[]},
     State};
handle_request(#'RequestInitChain'{}, State) ->
    {#'ResponseInitChain'{},
     State};
%% This counter implementation actually works only in serial mode, so we don’t support other values than “on”.
handle_request(#'RequestSetOption'{key="serial", value="on"}, State) ->
    {#'ResponseSetOption'{},
     State};
handle_request(#'RequestCommit'{},
               State=#state{chain_height=Height, chain_state=ChainState}) ->
    NewHeight = Height + 1,
    {#'ResponseCommit'{
        code='OK',
        data=encode_chain_state(ChainState)},
     State#state{chain_height=NewHeight}};
handle_request(#'RequestDeliverTx'{tx=Tx}, State=#state{chain_state=Counter}) ->
    CounterBin = binary:encode_unsigned(Counter),
    {Code, NewCounter} =
        case Tx of
            CounterBin ->
                {'OK', Counter + 1};
            _ ->
                {'BadNonce', Counter}
        end,
    {#'ResponseDeliverTx'{
         code=Code},
     State#state{chain_state=NewCounter}};
handle_request(#'RequestCheckTx'{tx=Tx}, State=#state{chain_state=Counter}) ->
    Code =
        case Tx >= Counter of
            true ->
                'OK';
            _ ->
                'BadNonce'
        end,
    {#'ResponseCheckTx'{
        code=Code,
        data= <<>>},
     State};
handle_request(#'RequestQuery'{path=Path},
               State=#state{chain_height=Height, chain_state=Counter}) ->
    Response =
        case iolist_to_binary(Path) of
            <<"tx">> ->
                #'ResponseQuery'{
                    code='OK',
                    value=binary:encode_unsigned(Counter)};
            <<"hash">> ->
                #'ResponseQuery'{
                   code='OK',
                   value=binary:encode_unsigned(Height)};
            _ ->
                #'ResponseQuery'{
                   code='OK',
                   log= "Invalid query path."}
        end,
    {Response,
     State}.
