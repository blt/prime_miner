#!/usr/bin/env escript
%%! -noshell -noinput

-define(TIMEOUT, 60000).
-define(INFO(Msg),      io:format(Msg)).
-define(INFO(Fmt,Args), io:format(Fmt,Args)).
-define(LTA(L),         list_to_atom(L)).

main([TargetNode, Cookie, ReleasePackage]) when is_list(TargetNode), is_list(Cookie) ->
    main([?LTA(TargetNode), ?LTA(Cookie), ReleasePackage]);
main([TargetNode, Cookie, ReleasePackage]) when is_atom(TargetNode), is_atom(Cookie) ->
    ok = start_distribution(TargetNode, Cookie),
    {ok, Vsn} = rpc:call(TargetNode, release_handler, unpack_release,
                         [ReleasePackage], ?TIMEOUT),
    ?INFO("Unpacked Release ~p~n", [Vsn]),
    {ok, OtherVsn, Desc} = rpc:call(TargetNode, release_handler,
                                    check_install_release, [Vsn], ?TIMEOUT),
    {ok, OtherVsn, Desc} = rpc:call(TargetNode, release_handler,
                                    install_release, [Vsn], ?TIMEOUT),
    ?INFO("Installed Release ~p~n", [Vsn]),
    ok = rpc:call(TargetNode, release_handler, make_permanent, [Vsn], ?TIMEOUT),
    ?INFO("Made Release ~p Permanent~n", [Vsn]);
main(_) ->
    ?INFO("usage: TODO~n"),
    init:stop(1).

start_distribution(TargetNode, Cookie) ->
    {ok, _Pid} = net_kernel:start([upgrader]),
    erlang:set_cookie(node(), Cookie),
    case {net_kernel:connect_node(TargetNode), net_adm:ping(TargetNode)} of
        {true, pong} ->
            ok;
        {_, pang} ->
            io:format("Node ~p not responding to pings.\n", [TargetNode]),
            init:stop(1)
    end.
