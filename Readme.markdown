# Erlang AMQP Examples

## Setup

You need to download the source code for 'rabbitmq-server' and 'rabbitmq-erlang-client' and create two symlinks to successfully compile.

- deps/rabbitmq-erlang-client => YOUR-SRC-DIR/rabbitmq-erlang-client
- deps/rabbitmq-erlang-client/rabbitmq-server => YOUR-SRC-DIR/rabbitmq-server

This was tested with rabbitmq-erlang-client source code as of changeset 681:9fe40c872e8d (Feb 14, 2010)

## Compile

cd rabbitmq-erlang-client-examples
rake compile

## Compile and Run AMQP Network Example Code

(assumes rabbitmq is already running on your system)

cd rabbitmq-erlang-client-examples
rake run

## Compile and Run AMQP Direct Example Code

(assumes that rabbitmq is *not* running elsewhere on your system.  This will start a new instance.)

Follow the instructions to make RabbitMQ and the RabbitMQ Erlang Client available to your Erlang install:
http://antoniogarrote.lacoctelera.net/post/2009/08/27/installing-rabbitmq-erlang-client-in-os-x

Start an Erlang Shell, and RabbitMQ as well.

rake run_direct


## References

http://antoniogarrote.lacoctelera.net/post/2009/08/27/installing-rabbitmq-erlang-client-in-os-x

http://medevyoujane.com/blog/2008/8/21/erlang-make-rake-and-emake.html

http://hopper.squarespace.com/blog/2008/1/12/introducing-the-erlang-amqp-client.html