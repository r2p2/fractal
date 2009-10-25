-module(fractal).

-export([render/1]).

render(Fractal) ->
  Height = length(Fractal),
  Width = length(lists:nth(1, Fractal)),
io:format("~p, ~p~n", [Height, Width]),
  Image = egd:create(Width, Height),
  render_lines(Image, Fractal, 0),
  Bin = egd:render(Image,png),
  egd:destroy(Image),
  Bin.

% Local Functions

render_lines(_, [], _) -> ok;
render_lines(Image, [Line|T], Y) ->
  render_pixel(Image, Line, 0, Y),
  render_lines(Image, T, Y+1).

render_pixel(_, [], _, _) -> ok;
render_pixel(Image, [{_, _, Value}|T], X, Y) ->
  egd:line(Image, {X, Y}, {X, Y}, egd:color(Image, Value)),
  render_pixel(Image, T, X+1, Y).
  






