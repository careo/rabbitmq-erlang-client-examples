-module(stocks_example).

-include_lib("../rabbitmq-erlang-client/rabbitmq_server/include/rabbit.hrl").
-include_lib("../rabbitmq-erlang-client/rabbitmq_server/include/rabbit_framing.hrl").
-include("../rabbitmq-erlang-client/include/amqp_client.hrl").

-export([start/0]).

start() ->
    amqp_lifecycle().
    
amqp_lifecycle() ->
    User = Password = "guest",
    Realm = <<"/data">>,

    %% Start a connection to the server

    Connection = amqp_connection:start(User, Password, "localhost"),

    %% Once you have a connection to the server, you can start an AMQP channel gain access to a realm

    Channel = amqp_connection:open_channel(Connection),
    Access = #'access.request'{realm = Realm,
                               exclusive = false,
                               passive = true,
                               active = true,
                               write = true,
                               read = true},
    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),

    %% Now that you have access to a realm within the server, you can declare a queue and bind it to an exchange

    Q = <<"my_stocks">>,
    X = <<"stocks">>,
    BindKey = <<"#">>,

    QueueDeclare = #'queue.declare'{ticket = Ticket, queue = Q,
                                    passive = false, durable = false,
                                    exclusive = false, auto_delete = false,
                                    nowait = false, arguments = []},
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = ConsumerCount}
                            = amqp_channel:call(Channel, QueueDeclare),
    log(message_count,MessageCount),
    log(consumer_count,ConsumerCount),

    ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
                                          exchange = X, type = <<"topic">>,
                                          passive = false, durable = false,
                                          auto_delete = false, internal = false,
                                          nowait = false, arguments = []},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
    QueueBind = #'queue.bind'{ticket = Ticket,
                              queue = Q,
                              exchange = X,
                              routing_key = BindKey,
                              nowait = false, arguments = []},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% The queue has now been set up and you have an open channel to so you can do something useful now ....

    setup_consumer(Channel, Ticket, Q),

    %% After you've finished with the channel and connection you should close them down

    ChannelClose = #'channel.close'{reply_code = 200, reply_text = <<"Goodbye">>,
                                    class_id = 0, method_id = 0},
    #'channel.close_ok'{} = amqp_channel:call(Channel, ChannelClose),
    ConnectionClose = #'connection.close'{reply_code = 200, reply_text = <<"Goodbye">>,
                                          class_id = 0, method_id = 0},
    #'connection.close_ok'{} = amqp_connection:close(Connection, ConnectionClose),
    ok.

send_message(Channel, Ticket, X, RoutingKey, Payload) ->
    BasicPublish = #'basic.publish'{ticket = Ticket,
                                    exchange = X,
                                    routing_key = RoutingKey,
                                    mandatory = false,
                                    immediate = false},
    Content = #content{class_id = 60,
         properties = amqp_util:basic_properties(),
         properties_bin = none,
         payload_fragments_rev = [Payload]
        },
    amqp_channel:cast(Channel, BasicPublish, Content).

setup_consumer(Channel, Ticket, Q) ->

    %% Register a consumer to listen to a queue

    BasicConsume = #'basic.consume'{ticket = Ticket,
                                    queue = Q,
                                    consumer_tag = <<"">>,
                                    no_local = false,
                                    no_ack = true,
                                    exclusive = false,
                                    nowait = false},
    #'basic.consume_ok'{consumer_tag = ConsumerTag}
                     = amqp_channel:call(Channel, BasicConsume, self()),

    %% If the registration was sucessful, then consumer will be notified

    receive
        #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
    end,

    %% When a message is routed to the queue, it will then be delivered to this consumer

    Msg = read_messages(0),
    io:format("Msg: ~p~n", [Msg]),
    
    %% After the consumer is finished interacting with the queue, it can deregister itself

    BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
                                  nowait = false},
    #'basic.cancel_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel,BasicCancel).

read_messages(Timeouts) ->
    receive
        {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=RoutingKey}, Content} ->
            #content{payload_fragments_rev = Payload} = Content,
            #content{properties_bin = PropertiesBin} = Content,
            #content{class_id = ClassId} = Content,
            #'P_basic'{content_type = ContentType} = rabbit_framing:decode_properties(ClassId, PropertiesBin),
            #'P_basic'{headers = Headers} = rabbit_framing:decode_properties(ClassId, PropertiesBin),
            io:format("RoutingKey received: ~p~n", [RoutingKey]),
            io:format("ContentType received: ~p~n", [ContentType]),
            io:format("Headers received: ~p~n", [Headers]),

            case ContentType of 
                <<"application/json">> ->
                    Payload2 = mochijson2:decode(Payload),
                    io:format("Payload2 received: ~p~n", [Payload2]);
                _ ->
                    io:format("Payload received: ~p~n", [Payload])
            end,

            _Properties = rabbit_framing:decode_properties(ClassId, PropertiesBin),

            io:format("~n", []),
            read_messages(0);
        Any ->
            io:format("Other: ~p~n", [Any]),
            read_messages(0)
            
    after 1000 ->
        case Timeouts of
            0 -> 
                Timeouts2 = Timeouts + 1,
                read_messages(Timeouts2);
            5 -> 
                io:format("Message timeout exceeded ~n"),
                exit(message_timeout_exceeded);
            _ ->
                Timeouts2 = Timeouts + 1,
                io:format("."),
                read_messages(Timeouts2)
        end
    end.

log(Key,Value) ->
    io:format("~p: ~p~n",[Key,Value]).