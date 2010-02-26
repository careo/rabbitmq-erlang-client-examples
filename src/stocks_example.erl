-module(stocks_example).

-include("../deps/rabbitmq-erlang-client/include/amqp_client.hrl").

-export([start/0]).

start() ->
    amqp_lifecycle().

amqp_lifecycle() ->

    %% Start a connection to the server
    Connection = amqp_connection:start_network(),

    %% Once you have a connection to the server, you can start an AMQP channel
    Channel = amqp_connection:open_channel(Connection),

    %% Now that you have access to a connection with the server, you can declare a queue and bind it to an exchange
    Q = <<"my_stocks">>,
    X = <<"stocks">>,
    BindKey = <<"#">>,

    QueueDeclare = #'queue.declare'{queue = Q,
                                    passive = false, durable = false,
                                    exclusive = false, auto_delete = false,
                                    nowait = false, arguments = []},
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = ConsumerCount}
                            = amqp_channel:call(Channel, QueueDeclare),
    log(message_count,MessageCount),
    log(consumer_count,ConsumerCount),

    ExchangeDeclare = #'exchange.declare'{exchange = X, type = <<"topic">>,
                                          passive = false, durable = false,
                                          auto_delete = false, internal = false,
                                          nowait = false, arguments = []},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = BindKey,
                              nowait = false, arguments = []},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Inject a sample message so we have something to consume later on.  For testing purposes only.
    log(send_message,"start"),
    RoutingKey = <<"test.routing.key">>,
    Payload = <<"This is a really interesting message!">>,
    send_message(Channel, X, RoutingKey, Payload),

    %% The queue has now been set up and you have an open channel to so you can do something useful now.
    log(setup_consumer,"start"),
    setup_consumer(Channel, Q),
    log(setup_consumer,"finished"),

    %% After you've finished with the channel and connection you should close them down
    log(channel_close,"start"),
    ChannelClose = #'channel.close'{reply_code = 200, reply_text = <<"Goodbye">>,
                                    class_id = 0, method_id = 0},
    #'channel.close_ok'{} = amqp_channel:call(Channel, ChannelClose),

    log(connection_close,"start"),
    ok = amqp_connection:close(Connection),
    log(connection_close,"Demo Completed!"),
    ok.

send_message(Channel, X, RoutingKey, Payload) ->
    log(send_message,"basic.publish setup"),
    BasicPublish = #'basic.publish'{exchange = X,
                                    routing_key = RoutingKey,
                                    mandatory = false,
                                    immediate = false},

    log(send_message,"amqp_channel:cast"),
    ok = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}).

setup_consumer(Channel, Q) ->

    %% Register a consumer to listen to a queue
    log(setup_consumer,"basic.consume"),
    BasicConsume = #'basic.consume'{queue = Q,
                                    consumer_tag = <<"">>,
                                    no_local = false,
                                    no_ack = true,
                                    exclusive = false,
                                    nowait = false},
    #'basic.consume_ok'{consumer_tag = ConsumerTag}
                     = amqp_channel:subscribe(Channel, BasicConsume, self()),

    %% If the registration was sucessful, then consumer will be notified
    log(setup_consumer,"basic.consume_ok start receive"),
    receive
        #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
    end,
    log(setup_consumer,"basic.consume_ok finished"),

    %% When a message is routed to the queue, it will then be delivered to this consumer
    log(read_messages,"start"),
    Msg = read_messages(0),
    io:format("Msg: ~p~n", [Msg]),
    log(read_messages,"finish"),

    %% After the consumer is finished interacting with the queue, it can deregister itself
    log(basic_cancel,"start"),
    BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
                                  nowait = false},
    #'basic.cancel_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel,BasicCancel).

read_messages(Timeouts) ->
    receive
        {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=RoutingKey}, Content} ->
            log(read_messages,"basic.deliver"),
            io:format("RoutingKey received: ~p~n", [RoutingKey]),
            #amqp_msg{payload = Payload} = Content,
            io:format("Payload received: ~p~n", [Payload]),
            read_messages(0);
        Any ->
            io:format("received unexpected Any: ~p~n", [Any]),
            read_messages(0)
    after 1000 ->
        case Timeouts of
            0 ->
                Timeouts2 = Timeouts + 1,
                read_messages(Timeouts2);
            5 ->
                io:format("~n"),
                io:format("Message timeout exceeded ~n");
            _ ->
                Timeouts2 = Timeouts + 1,
                io:format("."),
                read_messages(Timeouts2)
        end
    end.

log(Key,Value) ->
    io:format("~p: ~p~n",[Key,Value]).
