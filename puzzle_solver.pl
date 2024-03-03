% Name: Brandon Lim Vei Liang 
% Student id : 1430381
% Summary - 
% In the main predicate puzzle_solution(+Puzzle, +WordList), there are only two function calls, and those two predicates
% make up for the two major functionalities for this project: to extract all slots and fill them up with the WordList. 
% Firstly, extract_slots(+Puzzle, -Slots) calls 3 separate functions. transpose(+Puzzle, -NewPuzzle), to transpose the puzzle,
% append(Puzzle, NewPuzzle, RowsAndCols) combines the original puzzle and the transposed puzzle, and finally, 
% add_slots(+RowsAndCols, -Ans, -Slots), returns the slots extracted from the combined rows and columns. Then, there's
% solve_rows(+Slots, +WordList), which fills up the slots with the WordList. Finally, the print_puzzle(+Puzzle) is called
% to print the solved puzzle.

:- ensure_loaded(library(clpfd)).
% puzzle_solution(+Puzzle, +WordList), calls extract_slots(+Puzzle, -Slots) to extract all slots and solve_rows(+Slots, +WordList)
% to fill the slots up.
puzzle_solution(Puzzle, WordList):-
    extract_slots(Puzzle, Slots),
    solve_rows(Slots, WordList).
       
% extract_slots(+Puzzle, -Slot), takes in a puzzle and extracts all the possible slots by transposing them and adding the 
% transposed puzzle's slots to the original puzzle's slots
extract_slots(Puzzle, Slots):-
    transpose(Puzzle, NewPuzzle),
    append(Puzzle, NewPuzzle, RowsAndCols),
    add_slots(RowsAndCols, Ans, Slots).

% add_slots(+RowsAndCols, -Ans, -Slots) calls add_slot_within_row for each of the rows and columns in the argument.
% Ans is the accumulator and Slots is the final list of slots. The predicate ends when RowsAndCols is empty. 
add_slots([], FinalSlots, Ans):-
    Ans = FinalSlots.
add_slots([Row|Puzzle],Slots, Ans):-
    add_slot_within_row(Row, '#', [], NewSlots, FinalSlotsForRow),
    ( FinalSlotsForRow \= []
    -> append(Slots, FinalSlotsForRow, FinalSlots),
       add_slots(Puzzle, FinalSlots, Ans);
    add_slots(Puzzle, Slots, Ans)).

% add_slot_within_row(+Row, +LastSlot, -Slots, -NewSlots, -FinalSlotsForRow) takes in a specific Row, iterate through
% the Row, and extract any possible slots and compile it into FinalSlotsForRow. This predicate first checks if the 
% current slot is # or not, it then checks the previous slot (LastSlot) to see if it is # or not,
% if current and previous slot are both #, it iterates through while passing down Slot as [], if the current slot
% is # but last slot wasn't, 
add_slot_within_row([], _, [], FinalSlots, FinalSlots).
add_slot_within_row([], _, Slots, NewSlots, FinalSlots):-
    append(NewSlots, [Slots], FinalSlots). 
add_slot_within_row([X|Row], LastSlot, Slots, NewSlots, FinalSlots):-
    ( X == '#'
    -> ( LastSlot == '#'
       -> add_slot_within_row(Row, X, [], NewSlots, FinalSlots)
       ; ( Slots == []
         -> add_slot_within_row(Row, X, [], NewSlots, FinalSlots)
         ; append(NewSlots, [Slots], UpdatedSlots),
           add_slot_within_row(Row, X, [], UpdatedSlots, FinalSlots)))
    ;  ( LastSlot == '#'
       -> add_slot_within_row(Row, X, [], NewSlots, FinalSlots)
       ; ( Slots == []
         -> NewSlot = [LastSlot, X],
            add_slot_within_row(Row, X, NewSlot, NewSlots, FinalSlots)
         ; reverse(Slots, ReversedSlots),
           reverse([X|ReversedSlots], NewSlot),
           add_slot_within_row(Row, X, NewSlot, NewSlots, FinalSlots)))).

% solve_rows(+Slots, +WordList), finds which slot has the fewest matching words, compile all the matches,
% then fills up those slots first. After those slots are filled, remove the slot from Slots, and word from
% WordList, then makes the recursive call with the rest of the slots and words.
solve_rows([], _).
solve_rows(Slots, WordList):-
    %find_fewest_matching(Slots, WordList, Slot, Num, FinalSlot),
    %compile_matches(FinalSlot, WordList, AccResults, Result),
    find_fewest_matches(Slots, WordList, [], Num, WordsThatMatch, FinalSlot, Result),
    fill_words(FinalSlot, Result),
    remove_used_slots(FinalSlot, Slots, RestSlots),
    remove_used_words(FinalSlot, WordList, RestWords),
    %select(FinalSlot, Slots, RestSlots),
    solve_rows(RestSlots, RestWords).

% remove_used_slots(+UsedSlots, +Slots, -RestSlots) iterates through the UsedSlots, remove them
% from the original Slots with the select/3 predicate, and returns the rest of the slots (RestSlots)
remove_used_slots([], Slots, Final):-
    Final = Slots.
remove_used_slots([U1|UsedSlots], Slots, Final):-
    remove_used_slots(UsedSlots, Slots, Result),
    select(U1, Result, Final).

% remove_used_words(+UsedWords, +WordList, -RestWords) iterates through the UsedWords, remove them
% from the original WordList with the select/3 predicate, and returns the rest of the words (RestWords)
remove_used_words([], Words, Final):-
    Final = Words.
remove_used_words([W1|UsedWords], Words, Final):-
    remove_used_words(UsedWords, Words, Result),
    select(W1, Result, Final).

% find_fewest_matches(+Slots, +WordList, -Slot, -Num, ,-WordsThatMatch, -FinalSlot, -FinalWords) iterates through the slots, 
% see how many matches each Slot has with matching_list(+Length, +WordList, -Matches), and determines
% which slot has the least amount of matches and return it as FinalSlot, and return its matches as
% FinalWords.
find_fewest_matches([], _, _, 0, _, _, _):-
    fail.
find_fewest_matches([], _, Slot, Num, WordsThatMatch, FinalSlots, FinalWords):-
    FinalSlots = Slot,
    FinalWords = WordsThatMatch.

find_fewest_matches([X|Slots], WordList, Slot, Num, WordsThatMatch, FinalSlots, FinalWords):-
    length(X, LenX),
    ( WordList == []
    -> Temp is 0
    ; find_matches(LenX, WordList, Temp, Matches)),
    ( Temp = 0
    -> ( Slot \= []
       -> ( check_length(LenX, Slot)
          -> append(Slot, [X], SlotsSoFar),
             find_fewest_matches(Slots, WordList, SlotsSoFar, Num, WordsThatMatch, FinalSlots, FinalWords)
    ;  find_fewest_matches(Slots, WordList, Slot, Num, WordsThatMatch, FinalSlots, FinalWords)))
    ;
    ( Num = Temp
    ->  append(Slot, [X], SlotsSoFar),
        append(WordsThatMatch, Matches, WordsThatMatchSoFar),
        remove_used_words(Matches, WordList, SmallerWordList),
        find_fewest_matches(Slots, SmallerWordList, SlotsSoFar, Temp, WordsThatMatchSoFar, FinalSlots, FinalWords)
    ; (Temp < Num
      -> append([], [X], NewSlot),
         append([], Matches, NewMatches),
         remove_used_words(Matches, WordList, SmallerWordList),
         find_fewest_matches(Slots, SmallerWordList, NewSlot, Temp, NewMatches, FinalSlots, FinalWords)
      ;  find_fewest_matches(Slots, WordList, Slot, Num, WordsThatMatch, FinalSlots, FinalWords)))).
    
    
% find_matches(+Length, +WordList, -Result, -Matches), returns how many Matches the Length has
% in the WordList, as well as the words that match the Length in the WordList.
find_matches(Length, WordList, Result, Matches):-
    find_matches(Length, WordList, 0, Result, Matches).
find_matches(_, [], Count, Result, MatchList):-
    Result = Count,
    MatchList = [].
find_matches(Length, [X|WordList], Count, Result, MatchList):-
    length(X, Length),
    UpdatedCount is Count + 1,
    find_matches(Length, WordList, UpdatedCount, Result, MatchesSoFar),
    MatchList = [X|MatchesSoFar];
    find_matches(Length, WordList, Count, Result, MatchList).
    
% fill_words(+Slots, +WordList) iterates through Slots while filling each of them up
% with the WordList that has been passed in, with the member/2 predicate. It then deletes
% the word from the WordList and makes the recursive call.
fill_words([], _).
fill_words(_, []):-
    fail.
fill_words([S1|Slots], WordList):-
    member(S1, WordList),
    select(S1, WordList, SmallerResults),
    fill_words(Slots, SmallerResults).

% check_length(+LenSlot, +SlotList) checks the remaining slots in find_fewest_matches, to see
% if any of the slots in SlotList match the length of the current Slot.
check_length(_, []):-
    fail.
check_length(LenSlot, [X|SlotList]):-
    length(X, LenSlot);
    check_length(LenSlot, SlotList).

print_puzzle([]).
print_puzzle([Row|Rows]):-
    print(Row),
    nl,
    print_puzzle(Rows).

test(large) :-
    WordList = [[o, j, z, d, h, s, w], [m, t, g, e, j, n, v], [d, q, e, v, x, k, j, m, m, q, q, g, t], [x, n], [j, v], [g, e, x, x, b, a, c, v, s, b, f, q, q, s, d, d, f, g, u, p, h, x], [m, s, g, e], [e, k, m, n], [v, y, i, l, t, w, j], [c, j], [u, c, s, a, j], [n, q], [t, w, q, d], [o, g, c, l, v, b, y, m, z], [v, e], [v, z, j, w, g, u, q], [g, w, h, e, p], [d, j], [d, b, c, f], [o, h, g, x, v], [s, z], [b, u, y, g, f, a], [i, v, k], [f, m, z, p, g, a, q, b, b, e, m, w, g, c, w, s, l, d, g, e, p, w], [c, p, m, v, b, x], [c, u, z, o, c, h, d, g, m, b, f, w, b, f], [l, n, f, u, o, o, i, j, m, n, s, x], [g, g, d], [f, f, b, x, a, b, r, z, h, q, p, h, w], [g, n, g], [x, x], [p, b, n, a, l, p, t, f], [t, t, o, k, k, k, q, q, e, z, v, s], [p, a, t, t, o], [g, h, u, l, e], [d, t, q], [i, v, j, x, p, t, s, k, h], [m, q, f, i], [k, a, e, t, f, j, u, f, y, o, m], [d, k, k, g, s], [x, p, a, p, s, e, l], [n, c, q], [q, x, h, i, k, q], [n, f, a, f], [q, a, j, n, q, s, d], [e, z], [z, n, g, g, a, y, b, i, i, r], [y, j], [p, i, o, u, u, z, e], [x, n, s, h], [b, t, z, q, u], [e, s, z], [h, u], [h, m], [p, h, e, o, z], [i, m, m, j, q, b, j, r], [x, w, o, x, x, u, v, g, u, h, j], [q, e, d], [t, f, t, o, q, i, h, i, f, k], [t, z, e, h], [p, f, d, g, u], [w, k], [y, j, j, w, e, l], [u, s, k, d, q, f, m, s], [r, q, i, z, d, e], [m, n], [y, h], [u, z], [v, j, x, i, o, j, z], [z, z, n], [c, c], [p, y], [c, n, j, w, c, g], [r, b, n, a, l, k, d, w, x, d, m], [z, b, x, n], [l, x, d, d, s, a, e], [z, c, c, g, v], [k, a], [o, y, a, g, h, r, t], [c, t, l, w, e], [y, h, v, m, i], [l, o, y], [u, g], [q, z, l, c, d], [m, c, r, g, a, i], [p, z, e, a], [t, z, e, x, b, p], [t, w, p], [g, o], [d, f, x, m, t], [q, q, o, j, x], [s, b, o, k, o, v, n, y, o], [k, c, v, z, x, p, h, q, l, y], [h, e, j, q, d], [b, c, q, e, e, b], [r, y, r], [u, e, l, z, u, d, m, s, n, b], [h, l, k], [b, n, e, k], [f, m], [x, x, a, s, m, u, q], [w, c, o, x, u, f, t], [n, g, q, i, m, j], [q, q, i], [l, u], [i, x, q, t, j], [b, z, g, e, x, p], [j, k], [h, p, h, b, e, n, f, p], [w, y, r, c, j], [t, p, g, g, w, f, p, w, m, g, a], [r, t, m, a, c, n], [m, v, w, h, y, q, z], [e, a], [e, g, t, k, m, m, y, g, p, m], [o, n, y], [e, a, p, s, b, j, v, x], [z, j, h, i, h, q, i, t, h], [d, o, g], [j, y, l, w, z, p, a, g, g, c, q, m, z, f], [t, s], [i, s], [n, o, x, h, l, g, s, w, l], [v, a, h, k, r, m, b, p, c, z, f], [e, t, y, z, g], [j, q, p, e, c, c], [k, d, l, w, c, r], [n, z, u, w, k, d, s, p, g, r, n, q, t, d, c, y], [q, j], [o, g], [m, i, b, m, s, v, w, s, n, d, k, w, v], [j, y, u, f, s, c, b], [u, b, q, m], [w, g, s, c, e, b], [n, n, j, k, t, c, m, m, x, i, m], [g, p], [o, u, m, t, d, f, c, f, a, v, p, n, y, h, q, y, u], [k, u, x, y], [x, i, x, e, z, p, j], [s, w, j, m, u, f], [f, j, u, e, j], [r, a, g], [r, x, x], [b, j], [z, j, g, q], [z, z, b, t, i, d, a], [d, j, v, b], [t, x, y, a, q], [h, i, p], [d, v, e, d, d, p, o, x, t, v, k, f, p, h], [n, o], [r, s, t, t, e, i, s, e], [b, g, c, a, o, j, k], [i, m, t, e, v, a, y, q, p, k], [m, j, p, g, h], [c, q, q], [c, a, h, b, k, x, g, q, o], [f, l, j, l, a, z], [c, z, u], [g, t, q, n, c, j, m], [w, g, k, o, f, q, d, r, k, p, s], [u, p, t], [x, k, g, l, g, v], [q, b, g, k, i, o], [e, m, g], [b, g, z, k, t], [q, u, h, o, u, i, d, h, c, o, z, u], [z, w, m, t, x, k, o], [m, x, n, c, o, b, m, h, q, s], [a, z, e, q, s, o, w, r, d], [x, d], [g, f, m, h, h, d, g], [t, x], [l, h, e, b, q, q, k, w, j, e, o, i, k, j, x, t], [q, p, l], [e, p, y], [l, l], [g, b, q, v, g, m, f, p, e, h], [z, h, d, z, d], [h, z, w, x, w, g, d, g, w], [e, b], [b, x, w, w, h, z], [x, q, i], [i, q], [m, c, c, d, q, u, c, p, m, p, o, s, c, m, w], [j, c, v, y, v, g, b, w, v, m, p, s], [i, f, f], [t, r, f, l, d, o], [g, m, g, w, r, i, g], [n, v, y, m], [c, f], [s, q, a, d, j, m, k, m, z, z, l, g, x, y, m, x, j, a], [b, s], [v, s, i, z], [f, p], [s, z, b, w, a, m], [s, u, k], [o, j], [m, c], [b, l], [s, s], [f, p, i, s, e, x, j, t], [n, x, e, i, t, u, n, f], [a, n, y, v], [s, e], [d, f, t, v, z, l, l], [e, z, n, q, z], [b, t, h], [y, l, a, n, v, b], [q, q, w, e], [d, n, g, t, t, l], [s, b, e, r], [k, p, q, h], [p, r], [w, h, z, w], [e, q, j], [r, g, f, n, t], [z, h, j, h, q, c], [z, q, e, h], [h, t], [z, k, u, s, n], [v, s], [e, u, g, o, l, a, n], [i, c, l], [e, o, j, l, n, b, m], [p, r, w, n, n], [x, d, c, z], [p, o], [g, b], [x, y, a, j, q, k, g, e, a, e, a, m, k, d, j], [k, d, j, j, b, w, o, x, m, k], [g, t, x, p, d, p, d, h], [x, e], [q, n, c, a, g, b, d, k, k], [j, f], [w, u], [i, x], [a, n, a, z, w, f, e, y, d, v, h], [b, i, f, n, p, g, p, s, w, t], [m, g, u, g, y], [g, e, c, y, q, o, d], [s, m], [n, m, p], [s, c, c, p, v, c], [m, u, c, u, g, c, m, p, h, t, q, b, u, x, g, m, s, a, i, t, b], [e, j], [m, b, q, z, g], [q, p, s, q, f, p, n, b, u, f], [x, u, n], [z, o], [v, j, m, f, r, j, m], [q, h, a], [a, m, s, n, l, j, q, i, e, u], [e, k, b], [w, v, z], [n, y, x], [g, x, j, g], [v, x, a, e, u, x, r, s, v, w, y, n, z], [x, o, c, o, q, w, h, x, f, e, q, u, i], [w, i, b], [f, h], [z, g, k, h, j, c, l, b, v, q], [i, y, y], [t, t, f, m], [n, h, v, x, g, p, d, y, i, f], [w, c, o, p, n, e, f], [r, q], [t, y, d, s], [x, b, q, e, k], [g, t, t, o, k], [l, h], [u, c, g, y], [y, e, m, l, c, z], [s, z, c, c, g], [i, d, f, q, m, q, w], [z, g, v], [d, o, b], [u, j], [g, y, b, p]],
    Puzzle =  [[_,_,_,_,_,_,_,#,_,_,_,_,_,_,_,#,_,_,_,_,_,_,_,_,_,_,_,_,_,#,_,_],
                [_,#,_,_,#,#,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,#,#,_,#],
                [_,_,_,_,#,_,_,_,_,#,_,#,_,_,_,_,_,_,_,#,_,_,#,_,_,_,_,_,#,_,_,#],
                [_,_,_,_,#,#,_,_,_,_,_,_,_,_,_,#,_,_,#,_,_,_,_,_,_,_,#,_,_,_,_,_],
                [_,_,#,_,_,_,_,#,_,_,_,_,_,#,#,_,_,#,_,#,#,_,_,_,_,_,_,#,_,_,_,#],
                [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,#,#,_,_,_,_,_,_,#,_],
                [_,_,_,_,_,_,_,_,_,_,_,_,_,_,#,#,_,_,_,_,_,_,_,_,_,_,_,_,#,_,_,_],
                [_,_,_,_,_,_,_,_,_,_,_,_,_,#,#,_,#,_,_,_,#,_,_,#,_,_,_,_,_,_,_,_],
                [_,#,_,_,_,_,_,_,_,_,_,_,_,_,#,_,_,_,_,_,#,_,#,_,_,_,_,_,#,_,_,_],
                [_,#,_,_,_,_,_,_,_,_,_,#,_,_,_,_,#,_,#,_,#,_,_,_,_,_,_,_,_,_,_,_],
                [_,#,_,_,_,_,_,#,#,_,#,_,_,_,_,_,_,_,#,_,#,#,_,_,_,#,_,_,_,_,_,_],
                [_,_,_,_,#,_,#,_,_,_,_,_,_,_,#,_,_,#,_,_,_,_,_,_,_,_,_,_,#,_,#,_],
                [_,_,#,_,_,_,_,_,_,_,#,_,#,_,#,_,_,_,_,#,_,_,_,_,_,#,_,_,_,#,_,#],
                [_,_,#,_,_,#,_,_,_,_,_,#,_,_,_,_,_,_,_,_,#,_,_,_,_,_,_,_,_,_,_,_],
                [_,_,_,#,_,_,_,_,_,_,_,_,_,_,#,_,_,_,_,#,#,_,_,_,_,_,#,_,_,#,#,#],
                [_,_,_,_,_,_,#,_,_,_,_,_,_,_,_,#,#,_,_,_,_,_,_,#,_,_,#,_,_,#,_,_],
                [_,#,_,#,_,_,_,_,_,_,_,#,#,_,_,_,#,#,_,_,#,_,_,#,_,#,_,_,_,_,_,_],
                [#,_,_,_,_,_,_,_,_,_,_,_,#,_,_,_,_,#,#,_,_,_,_,_,_,_,#,_,_,_,_,_],
                [_,_,#,_,_,_,_,_,_,_,#,_,_,_,_,_,#,_,#,#,_,_,_,_,_,#,#,#,_,_,_,#],
                [_,_,#,#,_,_,_,_,_,#,_,_,_,_,_,_,#,_,_,_,_,#,_,#,_,_,_,_,_,_,#,_],
                [_,#,_,_,_,#,_,_,#,_,#,_,_,_,_,_,#,_,_,_,_,_,#,_,_,_,_,_,_,_,_,_],
                [_,#,_,#,_,_,_,_,_,_,_,_,_,_,#,_,#,_,_,_,_,_,#,#,#,#,_,_,_,_,_,_],
                [#,_,_,_,#,_,#,_,_,_,_,_,_,_,_,_,_,#,_,_,_,#,_,_,_,_,#,_,#,_,_,#],
                [_,_,_,_,_,_,_,#,#,#,_,_,_,_,_,_,_,#,#,_,_,_,_,_,_,#,_,_,_,#,_,_],
                [_,_,_,_,_,#,_,_,_,_,_,_,#,_,_,#,_,_,_,_,_,_,_,_,#,#,#,_,_,_,_,_],
                [_,#,#,_,_,_,_,_,_,_,_,_,_,_,#,_,#,_,_,_,_,_,_,#,_,_,_,_,_,_,_,#],
                [_,_,#,_,_,_,_,_,_,_,_,_,_,#,_,_,_,#,#,#,_,_,_,_,_,_,_,_,#,#,#,#],
                [_,_,_,_,_,_,_,_,_,#,_,_,_,#,_,_,_,_,_,_,_,_,_,_,_,_,_,_,#,_,_,#],
                [_,#,_,_,#,_,_,_,_,_,_,_,_,_,#,_,_,_,_,_,_,_,_,_,_,_,#,_,_,_,_,_],
                [_,_,_,_,_,_,#,_,_,_,_,_,_,#,#,#,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
                [#,#,#,#,_,_,#,_,_,#,#,_,_,_,_,_,_,_,_,_,_,_,_,_,#,_,_,_,_,_,_,_],
                [#,#,_,_,_,_,#,#,#,#,#,_,_,_,_,_,_,#,_,_,_,_,_,_,_,_,_,_,_,#,_,_]],
    time(puzzle_solution(Puzzle, WordList)),
    print_puzzle(Puzzle).
    
