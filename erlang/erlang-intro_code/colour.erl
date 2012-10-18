%% Example code produced "during" Advanced Programming lecture.  
%% Packing and unpacking of 8-bit colors 
%% Date: Oct 11, 2011
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(colour).
-export([pack8bit/3,unpack8bit/1]).

pack8bit(R,G,B) -> <<R:3,G:3,B:2>>.

unpack8bit(P) ->
    <<R:3,G:3,B:2>> = P,
    {R, G, B}.
