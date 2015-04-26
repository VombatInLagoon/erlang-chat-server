# erlang-chat-server
A simple chat server written in Erlang/OTP.

## To compile 
Run the `build` script from the base directory.

## To run
Start Erlang with `erl -pa ebin`

Run chat server: `application:start(chat).`

You can use a program such as telnet or netcat to connect to the 

server as a client and communicate with it: `telnet localhost 6667`
## References

[Learn You Some Erlang for Great Good!](http://learnyousomeerlang.com/)

[erl-chat-server](https://github.com/luisgabriel/erl-chat-server)


