:- use_module('../game').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Test - DFS - Check Victory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Board 2x2 - WHITE WINS
%   A B
% 1 ⬡ . 1
%  2 ⬢ ⬢ 2
%     A B
%
test_2x2_white() :- 
    Board = [['\u2b21',.],['\u2b22','\u2b22']],
    Player = 'WHITE',
    
    check_victory(Board, Player), ! 
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 2x2 - BLACK WINS
%   A B
% 1 ⬡ . 1
%  2 ⬡ ⬢ 2
%     A B
%
test_2x2_black() :- 
    Board = [['\u2b21',.],['\u2b21','\u2b22']],
    Player = 'BLACK',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 2x2 - NO ONE WINS
%   A B
% 1 ⬡ . 1
%  2 . ⬢ 2
%     A B
%
test_2x2_nowin() :- 
    Board = [['\u2b21',.],[.,'\u2b22']],
    Player = 'BLACK',
    
    not(check_victory(Board, Player)), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - BLACK WINS
%   A B C
% 1 ⬡ ⬢ ⬢ 1
%  2 ⬡ ⬡ ⬡ 2
%   3 ⬢ ⬢ ⬡ 3
%      A B C
%
test_3x3_black() :- 
    Board = [['\u2b21','\u2b22','\u2b22'],['\u2b21','\u2b21','\u2b21'],['\u2b22','\u2b22','\u2b21']],
    Player = 'BLACK',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - WHITE WINS
%   A B C
% 1 ⬢ ⬢ ⬡ 1
%  2 ⬡ ⬢ ⬢ 2
%   3 ⬡ ⬡ ⬡ 3
%      A B C
%
test_3x3_white() :- 
    Board = [['\u2b22','\u2b22','\u2b21'],['\u2b21','\u2b22','\u2b22'],['\u2b21','\u2b21','\u2b21']],
    Player = 'WHITE',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.


% Board 3x3 - WHITE WINS
%   A B C
% 1 . ⬡ ⬢ 1
%  2 ⬡ ⬢ . 2
%   3 ⬢ ⬡ . 3
%      A B C
%
test_3x3_white1() :- 
    Board = [[.,'\u2b21','\u2b22'],['\u2b21','\u2b22',.],['\u2b22','\u2b21',.]],
    Player = 'WHITE',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - BLACK WINS
%   A B C
% 1 . ⬢ ⬡ 1
%  2 ⬢ ⬡ . 2
%   3 ⬡ ⬢ . 3
%      A B C
%
test_3x3_black1() :- 
    Board = [[.,'\u2b22','\u2b21'],['\u2b22','\u2b21',.],['\u2b21','\u2b22',.]],
    Player = 'BLACK',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - BLACK WINS
%   A B C D E
% 1 ⬡ ⬡ ⬡ ⬡ ⬡ 1
%  2 ⬢ ⬢ . ⬢ ⬡ 2
%   3 ⬡ ⬡ ⬡ . ⬡ 3
%    4 ⬡ ⬢ ⬡ ⬡ ⬡ 4
%     5 ⬡ ⬢ ⬢ . . 5
%        A B C D E
%
test_5x5_black() :- 
    Board = [
        ['\u2b21','\u2b21','\u2b21', '\u2b21', '\u2b21'],
        ['\u2b22','\u2b22',., '\u2b22', '\u2b21'],
        ['\u2b21','\u2b21','\u2b21', ., '\u2b21'],
        ['\u2b21','\u2b22','\u2b21', '\u2b21', '\u2b21'],
        ['\u2b21','\u2b22','\u2b22', ., .]],
    Player = 'BLACK',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - WHITE WINS
% A B C D E
% 1 ⬢ ⬢ ⬢ ⬢ ⬡ 1
%  2 ⬢ ⬡ . ⬢ ⬡ 2
%   3 ⬢ ⬡ ⬢ . ⬢ 3
%    4 ⬢ ⬡ ⬢ ⬢ ⬡ 4
%     5 ⬢ ⬡ ⬢ . . 5
%        A B C D E
%
test_5x5_white() :- 
    Board = [
        ['\u2b22','\u2b22','\u2b22', '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21','\u2b22', ., '\u2b22'],
        ['\u2b22','\u2b21','\u2b22', '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21','\u2b22', ., .]],
    Player = 'WHITE',
    
    check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - NO ONE WINS
%   A B C D E
% 1 ⬢ ⬢ . ⬢ ⬡ 1
%  2 ⬢ ⬡ . ⬢ ⬡ 2
%   3 . . . . .   3
%    4 ⬢ ⬡ . ⬢ ⬡ 4
%     5 ⬢ ⬡ . ⬢ ⬡ 5
%        A B C D E
%
test_5x5_nowin() :- 
    Board = [
        ['\u2b22','\u2b22',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        [.,.,., ., .],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22','\u2b21']],
    Player = 'WHITE',
    
    not(check_victory(Board, Player)), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% RUN ALL TESTS
:-  
    write('\e[35mRunning DFS tests...\e[0m\n'),
    test_2x2_white(), test_2x2_black(), test_2x2_nowin(), 
    test_3x3_black(), test_3x3_white(), test_3x3_white1(), test_3x3_black1(), 
    test_5x5_black(), test_5x5_white(), test_5x5_nowin(),
    write('\e[32mALL TESTS PASSED! :D\e[0m\n\n'), !
    ;   % OR
    write('\e[33m\nSOME TEST FAILED! :(\e[0m\n\n'), fail.
