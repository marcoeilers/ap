%% Use the mr as a wc.
-module(mr_wc).
-compile(export_all).
-define(otherwise, true).

%% Count the sum of all the words in the given list of tracks.
count(MR, Tracks) ->
    {ok, Sum} = mr:job(MR,
		       fun(Track) ->
			       {_,_,WordBags} = read_mxm:parse_track(Track),
			       lists:foldl(fun({_,Cnt}, Sum) -> Sum+Cnt end, 0, WordBags)
		       end,
		       fun(WordsInTrack, Total) ->
			       Total+WordsInTrack
		       end,
		       0,
		       Tracks),
    io:format("Counted ~B tracks. Size of result is ~B~n", [length(Tracks), Sum]),
    Sum.

%% Compute the average number of different words in a song _and_ the
%% average total number of words in a song.
compute_averages(MR, {Words, Tracks}) ->
    {ok, {AvgDiff, AvgWords, _}} = mr:job(MR,
					  fun(Track) ->
						  %% Output: Number of different words + Number of words in song
						  {_,_,WordBags} = read_mxm:parse_track(Track),
						  lists:foldl(fun({_,Cnt},{L,Sum}) -> {L+1,Sum+Cnt} end, {0,0},WordBags)
					  end,
					  fun({NDiffWords, NWords}, {AvgDiffWords,AvgWords,NTracks}) ->
						  TotalDiff = AvgDiffWords*NTracks + NDiffWords,
						  TotalWords = AvgWords*NTracks + NWords,
						  NNTracks = NTracks + 1,
						  {TotalDiff/NNTracks, TotalWords/NNTracks, NNTracks}
					  end,
					  {0,0,0},
					  Tracks),
    {AvgDiff, AvgWords}.

%% For a given word, find the MSD track ID's for all songs with that word.
%% Words: List of words
%% Tracks: List of tracks - 
%% Mapper:  Output Song Id + plus list of word indices
%% Reducer: Figure if our given song should be added to the list of
%%          songs containing WordIdx

grep(MR, Word, {Words, Tracks}) ->
    WordIdx = index_of(Word, Words),
    {ok, Result} = mr:job(MR,
			  fun(Track) ->
				  {_, MXMID, WordBags} = read_mxm:parse_track(Track),
				  {Ids, _} = lists:unzip(WordBags),
				  {MXMID, Ids}
			  end,
			  fun({MXMID, Ids}, ListOfTracks) ->
				  case lists:any(fun(Id) -> WordIdx == Id end, Ids) of
				      true ->
					  [MXMID|ListOfTracks];
				      false ->
					  ListOfTracks
				  end
			  end,
			  [],
			  Tracks),
    Result. %% 8188 songs contain the word "love"


%% Compute a reverse index, that is, a mapping from words to songs where they occur
%%
%% Mapper:  Same as grep, output {SongId, [Index]}
%% Reducer: Fold the id's over the dictionary, first getting the word
%%          (by index), then updating the dictionary using Word as key
%%          appending the song id if it already exists, otherwise
%%          start a new list.
reverse_index(MR, {Words, Tracks}) ->
    {ok, Result} = mr:job(MR,
			  fun(Track) ->
				  {_, MXMID, WordBags} = read_mxm:parse_track(Track),
				  {Idxs,_} = lists:unzip(WordBags),
				  {MXMID, Idxs}
			  end,
			  fun({MXMID, Idxs}, Dict) ->
				  lists:foldl(fun(Idx, D) ->
						      Word = lists:nth(Idx, Words),
						      dict:update(Word, fun(L) -> [MXMID|L] end, [MXMID], D)
					      end,
					      Dict,
					      Idxs)
			  end,
			  dict:new(),
			  Tracks),
    Result.

%% > RevIdx = mr_wc:reverse_index(MR, {Words, Tracks}).
%% > length(dict:fetch("love", RevIdx)). #=> 8188

%%% Utility functions
index_of(Item, List) ->
    index_of(Item, List, 1).

index_of(_, [], _) ->
    -1;
index_of(Item, [Item|_], Idx) ->
    Idx;
index_of(Item, [_|Tail], Idx) ->
    index_of(Item, Tail, Idx+1).
		       
