-module(gui_client).

-export([start/0]).

start() -> 
    S = gs:start(),
    gs:create(window,win1,S,[{width,300},{height,100}]),
    gs:create(label,lable_server_node,win1,[{label, {text,"server node:"}}]),
    gs:create(editor, editor_server_node, win1, [{x,100},{y, 5},{width,190},{height,20},{insert,{'end',"fs@ares"}}]),
    gs:create(button, button_connect, win1, [{label, {text, "connect to frame server node"}},
                                             {x,10}, {y,35}, {width, 280}, {height, 20}]),
    gs:config(win1, {map,true}),
    loop().     

loop() ->
    receive
        {gs, b1, click, Data, Args} ->
            io:format("Hello World!~n",[]),
            loop()
    end.

