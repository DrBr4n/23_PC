-module(jogo).
-export([start/0, participa/1, adivinha/2]).

start() -> spawn(fun() -> jogo([]) end).

participa(Jogo) ->
    Jogo ! {participa, self()},
    receive {Partida, Jogo} -> Partida end.

adivinha(Partida, N) ->
    Partida ! {adivinha, N, self()},
    receive {Res, Partida} -> Res end.

%jogo(L) when length(L) =:= 4 -> 
jogo([_,_,_,_] = Jogadores) -> 
    Num = rand:uniform(100) + 1,
    Partida = spawn(fun() -> partida(Num) end),
    [Jogador ! {Partida, self()} || Jogador <- Jogadores],
    jogo([]);

jogo(Jogadores) ->
    receive 
        {participa, Jogador} -> jogo([Jogador | Jogadores])
    end.

partida (Num) ->
    Self = self(),
    spawn(fun() -> receive after 60000 -> Self ! timeout end end),
    %spawn(fun() -> receive after 60000 -> self() ! timeout end end), isto ta errado pq o self() e o pid do lambda e nao da partida
    partida(Num, 0, false, false).

partida(Num, Tentativas, Ganhou, Timeout) ->
    receive
        {adivinha, N, Jogador} ->
        Res =
        if
            Ganhou -> "PERDEU";
            Timeout -> "TEMPO";
            Tentativas >= 100 -> "TENTATIVAS";
            N =:= Num -> "GANHOU";
            N > Num -> "MAIOR";
            N < Num -> "MENOR"
        end,
        Jogador ! {Res, self()},
        Ganhou1 = Ganhou orelse Res =:= "GANHOU",
        partida(Num, Tentativas + 1, Ganhou1, Timeout);
        timeout ->
            partida(Num, Tentativas, Ganhou, true)
    end.
