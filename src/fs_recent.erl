%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_recent).
-author("Michael Taylor <michael@emte.net.au>").

-export([request/2]).

request(Req, User) ->
	Data = [
		{user, User},
		{legs, legs()}
	],
    {ok, Body} = fs_recent_dtl:render(Data),
    Req:ok({"text/html", Body}).

legs() ->
	Query = "select Leg.leg
, Leg.created_time
, Leg.leg_gateway
, Leg.leg_number
, Leg.destination
, Leg.answered_time
, Leg.hangup_time
, Bridge.start_time
, Bridge.finish_time
, Bridge.is_transfer
, B.leg_gateway
, B.leg_number
 from Leg
 left join Bridge on Bridge.leg = Leg.leg
 left join Leg B on B.leg = Bridge.to_leg
 where Leg.is_originator = 1
 order by Leg.created_time desc
, start_time
, finish_time
 limit 30;",
	{data, Result} = mysql_pool:fetch(Query),
	Rows = mysql:get_result_rows(Result),
	legs(Rows, 0, []).

legs([], _Prev, Acc) ->
	lists:reverse(Acc);
legs([Row | Rows], Prev, Acc) ->
	{This, Data} = leg(Row, Prev),
	legs(Rows, This, [Data | Acc]).

leg(Row = [Leg | _], Leg) ->
	{Leg, [
		{at, ""},
		{caller, ""}
	|	leg(Row)
	]};
leg(Row = [Leg, {datetime, At}, _AGateway, ANumber | _], _) ->
	{Leg, [
		{at, datetime_string(At)},
		{caller, pretty(ANumber)}
	|	leg(Row)
	]}.

leg([_Leg, _AtTime, _AGateway, _ANumber, _Destination, _AnsweredTime, _HangupTime, {datetime, Start}, _FinishTime, 1, _BGateway, BNumber]) ->
	[
		{duration, "<b>Transfer</b>"},
		{called, pretty(BNumber)},
		{start, time_string(Start)},
		{finish, ""}
	];
leg([_Leg, _AtTime, _AGateway, _ANumber, _Destination, _AnsweredTime, _HangupTime, {datetime, Start}, {datetime, Finish}, _IsTransfer, _BGateway, BNumber]) ->
	StartSeconds = calendar:datetime_to_gregorian_seconds(Start),
	FinishSeconds = calendar:datetime_to_gregorian_seconds(Finish),
	DurationTime = calendar:seconds_to_time(FinishSeconds-StartSeconds),
	[
		{duration, time_string(DurationTime)},
		{called, pretty(BNumber)},
		{start, time_string(Start)},
		{finish, time_string(Finish)}
	];
leg([_Leg, _AtTime, _AGateway, _ANumber, _Destination, {datetime, Start}, {datetime, Finish}, _, _, _, _, _]) ->
    StartSeconds = calendar:datetime_to_gregorian_seconds(Start),
    FinishSeconds = calendar:datetime_to_gregorian_seconds(Finish),
    DurationTime = calendar:seconds_to_time(FinishSeconds-StartSeconds),
    [
        {duration, time_string(DurationTime)},
        {called, "<b>Voicemail</b>"},
        {start, time_string(Start)},
        {finish, time_string(Finish)}
    ];
leg([_Leg, _AtTime, AGateway, _ANumber, Destination | _]) ->
    {Duration, Called} = case AGateway of
        <<>> ->
            {"<b>Failed</b>", Destination};
        _ ->
            {"<b>No Answer</b>", AGateway}
    end,
    [
        {duration, Duration},
        {called, pretty(Called)},
        {start, ""},
        {finish, ""}
    ].


pretty(Number) when is_binary(Number) ->
	pretty(binary_to_list(Number));
pretty("200") -> "<b>200</b> Michael";
pretty("201") -> "<b>201</b> Assistant";
pretty("210") -> "<b>210</b> Ian";
pretty("211") -> "<b>211</b> Ian";
pretty("220") -> "<b>220</b> Alan";
pretty([$1, $3, A, B, C, D]) ->
	[$1, $3, A, $ , B, C, D];
pretty([$1, X, $0, $0, A, B, C, D, E, F]) ->
	[$1, X, $0, $0, $ , A, B, C, $ , D, E, F];
pretty([$0, $4, A, B, C, D, E, F, G, H]) ->
	[$0, $4, A, B, $ , C, D, E, $ , F, G, H];
pretty([$0, A, B, C, D, E, F, G, H, I]) ->
	[$(, $0, A, $), $ , B, C, D, E, $ , F, G, H, I];
pretty([A, B, C, D, E, F, G, H]) ->
	[$(, $0, $3, $), $ , A, B, C, D, $ , E, F, G, H];
pretty("gotalk-michael") -> "Omniscient Australia Pty Ltd";
pretty("gotalk-ian") -> "Cosimo Investments";
pretty("gotalk-alan") -> "ABL Accounting";
pretty(Number) -> mochiweb_html:escape(Number).

time_string({H, N, S}) ->
	io_lib:format("~2..0B:~2..0B:~2..0B", [H, N, S]);
time_string({_, {H, N, S}}) ->
	io_lib:format("~2..0B:~2..0B:~2..0B", [H, N, S]).

datetime_string({{Y, M, D}, {H, N, S}}) ->
	io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, N, S]).
