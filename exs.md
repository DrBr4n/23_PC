

## Teste  2022
### Parte 2

	Pretende-se que escreva em Java, fazendo uso das primitivas baseadas em monitores, código que permita o agrupamento, aos pares, de threads que tem o papel de produtor ou consumidor sobre um bounded-buffer criado para cada par de threads. Para tal, implemente a seguinte interface:
```java
interface MatchMaker {
	BoundedBuffer waitForConsumer();
	BoundedBuffer waitForProducer();
}
```
	A operação waitForConsumer(), para ser utilizada por uma thread com o papel de produtor, deverá bloquear até este produtor poder ser emparelhado com um consumidor(uma thread que tenha invocado ou vá invocar o waitForProducer), devolvendo um BoundedBuffer, que deverá ser criado para uso exclusivo deste par de threads. De igual modo, waitForProducer, para ser usada por um consumidor, deverá bloquear até este poder ser emparelhado com um produtor. 
	Assuma um número arbitrário de threads que possa querer ser emparelhada, faca o emparelhamento por ordem de chegada, e evite acordar threads sem necessidade. Assuma a existência de uma classe BoundedBuffer com um construtor por omissao.
```java
import java.util.*;
public class AMatchMaker //implements MatchMaker {
	public static void main(String argv[]) { System.out.println("Compiles"); }

	static class Cell {
		BoundedBuffer = b;
		boolean ready = false;

		synchronized boolean notReady() { return !ready; }

		synchronized BoundedBuffer get() {
			if (b == null) b = new BoundedBuffer();
			ready = true;
			return b;
		}
	}
	
	Queue<Cell> producers = new ArrayDeque<>();
	Queue<Cell> consumers = new ArrayDeque<>();

	BoundedBuffer waitForConsumer() throws InterruptedException {
		Cell c = consumers.peek();
		if (c == null) {
			c = new Cell();
			producers.add(c);
			while(c.notReady()) c.wait();
		} else {
			consumers.remove();
			c.notify();
		}
		return c.get();
	}

	BoundedBuffer waitForProducer() throws InterruptedException {
		Cell c = producers.peek();
		if (c == null) {
			c = new Cell();
			consumers.add(c);
			while(c.notReady()) c.wait();
		} else {
			producers.remove();
			c.notify();
		}
		return c.get();
	}
}
```

### Parte 3
	Apresente o código Erlang para implementar o mesmo cenário descrito no grupo 2, através de um ou mais processos. Suponha que os clientes sao processos Erlang que comunicam pelo mecanismo nativo de mensagens, e implemente tambem as funções de interface apropriadas para serem usadas por estes para interagirem com o(s) processos relevantes(s).
```erlang
-module(matchMaker).
-export([start/0, waitForConsumer/0, waitForProducer/0]).
% interface functions
start() ->
	register(?MODULE, spawn(fun() -> matchMaker([],[]) end)).
waitForConsumer() ->
	?MODULE ! {waitForConsumer, self()}.
waitForProducer() ->
	?MODULE ! {waitForProducer, self()}.

% private functions
matchMaker([C|Cs], [P,Ps]) ->
	C ! P ! boundedBuffer(),
	matchMaker(Cs, Ps).

matchMaker(Cs, Ps) ->
	receive
		{waitForConsumer, P} ->
			matchMaker(Cs, [P | Ps]);
		{waitForProducer, C} ->
			matchMaker([C | Cs], Ps)
	end.
```


## Teste 2019
### Parte 2
	Pretende-se que escreva em Java, fazendo uso de primitivas baseadas em monitores, código que permita jogadores participarem num jogo de adivinha. Cada partida envolve 4 jogadores (cada um representado por uma thread), que competem para ver quem adivinha primeiro um numero gerado aleatoriamente entre 1 e 100. Cada partida é limitada a um minuto e a 100 tentativas de respota (total para todos os jogadores). Devem ser suportadas varias partidas a decorrer em simultaneo. As interfaces a implementar sao:
```java
interface Jogo { Partida participa(); }
interface Partida { String adivinha(int n); }
```
	A operação participa() devera bloquear ate poder começar uma partida (4 jogadores a quererem participar), devolvendo o objeto que representa a partida. Sobre este objeto, a operação adivinha(n), usada para jogar, devolve um GANHOU se esta tentativa foi a primeira a acertar (dentro dos limites de tempo e tentativas de resposta); PERDEU se algum jogador ja ganhou; TEMPO se esgotou o limite de tempo da partida; MAIOR/MENOR se o número escondido esta acima/abaixo de n.
```java
import java.util.Random;
interface Jogo { Partida participa() throws InterruptedException; }
interface Partida { String adivinha(int n); }

class JogoImpl implements Jogo {
    PartidaImpl p = new PartidaImpl();
    int jogadores = 0;

    public synchronized Partida participa() throws InterruptedException {
        PartidaImpl ps = p;
        jogadores++;
        if (jogadores == 4) {
            notifyAll();
            jogadores = 0;
            ps.start();
            p = new PartidaImpl();
        } else {
            while (p = ps) wait();
        }
        return ps;
    }
}

class PartidaImpl implements Partida {
    int tentativas = 0;
    boolean ganhou = false;
    boolean timeout = false;
    int numero;

    public void start() throws InterruptedException {
        numero = new Random().nextInt(100);
        new Thread(() -> {
            Thread.sleep(60000);
            timeout();
        }).start();
    }

    synchronized void timeout() {
        timeout = true;
    }

    public synchronized String adivinha(int n) {
        if (ganhou) return "PERDEU";
        else if (tentativas >= 100) return "TENTATIVAS";
        else if (timeout) return "TEMPO";
        else {
            tentativas++;
            if (n > numero) return "MENOR";
            else if (n < numero) return "MAIOR";
            else {
                ganhou = true;
                return "GANHOU";
            }
        }
    }
}
```

### Parte 3
	Apresente o código Erlang para implementar o mesmo sistema descrito no grupo 2, atraves de um ou mais processos. Supondo que os jogadores sao processos Erlang que comunicam pelo mecanismo nativo de mensagens, implemente tambem as funcoes de interface apropriadas para serem usadas por estes para interagirem com o(s) processos relevantes(s).
```erlang
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
```

## Teste 2016
### Parte 2
	Pretende-se que escreva em Java, fazendo uso de primitivas baseadas em monitores, uma classe para monitorização de eventos num ambiente multithreaded em que os métodos a disponibilizar são a sinalização e a espera por eventos.
```java
void sinaliza(int tipo);
void espera(int tipo1, int n1, int tipo2, int n2);
```
	O método espera devera bloquear ate serem sinalizados, depois do momento em que e invocado: n1 vezes um evento tipo1 e n2 vezes um evento tipo2. Considere que o tipo de um evento é um inteiro entre 1 e E, especificado no construtor.
```java
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class MeuMonitor {
    Lock lock = new ReentrantLock();
    Condition notThere = lock.newCondition();
    int tipo1, n1, tipo2, n2;
    MeuMonitor() { tipo1 = 1; tipo2 = 2; n1 = 0; n2 = 0; }

    void sinaliza(int tipo) {
        lock.lock();
        try{
            if (tipo == tipo1) { n1 += 1; }
            if (tipo == tipo2) { n2 += 1; }
            notThere.signalAll();
        } finally {
            lock.unlock();
        }
    }
 
    void espera(int tipo1, int n1, int tipo2, int n2) throws InterruptedException {
        lock.lock();
        try{
            System.out.println("Starting to wait");
            while (!(this.n1 > this.n2) || !(this.n1 == n1 && this.n2 == n2)) {
                notThere.await();
            }
            System.out.println("Stopped waiting");
        } finally {
            lock.unlock();
        }
    }
}
```

### Parte 3
	Apresente o código em Erlang de um processo servidor relativamente a mesma situação descrita no grupo 2. Suponha que os clientes são processos Erlang, que comunica, pelo mecanismo nativo de mensagens, e implemente também as funções de interface apropriadas para ser rem usadas por estes.
```Erlang
-module(eventos).
-export([start/0, sinaliza/2, votos/2, espera/1]).

start() ->
    register (espera, spawn(eventos, espera, [false])),
    Pid = spawn(eventos, votos,  [0,0]),
    Pid ! tipo1,
    timer:sleep(3000),
    Pid ! tipo2,
    Pid ! tipo2,
    timer:sleep(3000),
    Pid ! tipo1,
    timer:sleep(3000),
    Pid ! tipo1,
    timer:sleep(3000),
    Pid ! votos,
ok.

sinaliza(Tipo, Pid) ->
    Pid ! {Tipo}.

votos(T1, T2) ->
    receive
        tipo1 -> 
            espera ! {T1+1,T2},
            votos(T1+1,T2);
        tipo2 -> 
            espera ! {T1,T2+1},
            votos(T1,T2+1);
        votos -> 
            io:format("Votos sao t1: ~p, t2: ~p. ~n",[T1,T2]),
            votos(T1,T2)
    end.

espera(true) -> io:format("Sai da espera~n",[]);
espera(false) -> 
    io:format("ainda espero~n",[]),
    receive
        {T1,T2} ->
            Res = if
                T1 == 3 andalso T2 == 2 -> true;
                T1 < 3 -> false;
                T2 < 2 -> false
            end,
    espera(Res)
end.
```

## Época Especial 2016
### Parte 2
	Considere um sistema de controlo de acesso a uma ponte pedonal, utilizada para visitar um ilhéu. Podem circular pessoas em ambos os sentidos ao mesmo tempo, mas só podem estar no máximo 10 pessoas em cima da ponte. Pretende-se que escreva em Java, fazendo uso de primitivas baseadas em monitores, uma classe para ser usada por threads que representam visitantes, em que os métodos a disponibilizar são: 
```java
void inicioTravessiaIda();
void inicioTravessiaVolta();
void fimTravessia();
```
	Os dois primeiros deverão bloquear até ser possível iniciar a travessia da ponte. O último é usado quando uma pessoa termina a travessia. Caso estejam pessoas a querer atravessar em ambos os sentidos, deverá ser dada prioridade a quem quer regressar da visita. 

```java
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Ponte {
    int ocupacao=0;
    Lock lock = new ReentrantLock();
    Condition canReturn = lock.newCondition();
    Condition canGo = lock.newCondition();
    
    void inicioTravessiaIda() throws InterruptedException {
        lock.lock();
        try{
            if (ocupacao == 10) {
                System.out.println("Ponte cheia");
                wait(); 
            }
            ocupacao +=1;
            Thread.sleep(5000);
            fimTravessia();
        } finally { lock.unlock(); }
    }

    void inicioTravessiaVolta() throws InterruptedException{
        lock.lock();
        try{
            if (ocupacao == 10) {
                System.out.println("Ponte cheia");
                wait(); 
            }
            ocupacao +=1;
            Thread.sleep(5000);
            fimTravessia();
        } finally { lock.unlock(); }
    }

    void fimTravessia() throws InterruptedException{
        lock.lock();
        try {
            ocupacao-=1;
            System.out.println("Pode entrar um");           
            canReturn.signal();
            canGo.signal();
        } finally { lock.unlock(); }
    }
}
```

## Exame 2016
### Parte 2
	Pretende-se que escreva em Java, fazendo uso de primitivas baseadas em monitores, uma classe para sondagens num ambiente multithreaded, em que os métodos a disponibilizar são: 
```java
void vota(String candidato);
void espera(String c1, String c2, String c3);

```
	O método vota permite a votação num candidato arbitrário (o conjunto de candidatos não tem que ser pré-definido); o método espera deve bloquear até os números de votos nos três candidatos em argumento formarem uma sequência crescente, ou seja V(c1) < V(c2) < V(c3). 
```java
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Exame {
    int c1,c2,c3;
    String cd1, cd2, cd3;

    Lock lock = new ReentrantLock();
    Condition notThere = lock.newCondition();

    Exame(String a, String b, String c){
        this.cd1 = a;
        this.cd2 = b;
        this.cd3 = c;
        this.c1 = 0; this.c2 = 0; this.c3 = 0;
    }
    
    void vota(String candidato){
        lock.lock();
        try {
            if (candidato.compareTo(cd1) == 0) { c1 += 1; }
            if (candidato.compareTo(cd2) == 0) { c2 += 1; }
            if (candidato.compareTo(cd3) == 0) { c3 += 1; }
            notThere.signalAll();
        } finally { lock.unlock(); }
    }

    void espera (String a, String b, String c)  throws InterruptedException{
        lock.lock();
        try {
            if (a.compareTo(cd1) != 0) return;
            if (b.compareTo(cd2) != 0) return;
            if (c.compareTo(cd3) != 0) return;
            while ((c1 < c2 && c2 < c3) != true) { notThere.await(); }
        } finally { lock.unlock(); }
    }

    void showVotes() {
        System.out.println("Votos-----------------");
        System.out.println(cd1 + ": " + c1);
        System.out.println(cd2 + ": " + c2);
        System.out.println(cd3 + ": " + c3);
        System.out.println("----------------------");
    }
}

class Monitor {
    public static void main(String[] args) {
        Exame teste = new Exame("tiago", "sofia", "rosa");

        new Thread(() -> {
            try {
                System.out.println("Vou entrar...");
                teste.espera("tiago", "sofia", "rosa");
                System.out.println("1 - sai da espera finalmente!!!");
            } catch (Exception e) {}
        }).start();

        new Thread(() -> {
            try{
                System.out.println("Vou votar...");
                teste.showVotes();
                Thread.sleep(1000);
                teste.vota("tiago");
                teste.vota("rosa");
                teste.vota("sofia");
                teste.showVotes();
                Thread.sleep(1000);
                teste.vota("rosa");
                System.out.println("o 2 deve sair aqui");
                teste.vota("rosa");
                teste.showVotes();
                Thread.sleep(1000);
                teste.vota("sofia");
                System.out.println("o 1 deve sair aqui");
                teste.showVotes();
                Thread.sleep(1000);
                teste.showVotes();
            } catch (Exception e) { }
        }).start();
    }
}
```

### Parte 3
Apresente o código Erlang de um processo servidor relativamente à mesma situação descrita no grupo 2, com a diferença que agora se pretende esperar até que os votos em c3 sejam mais do que em c1 e mais do que em c2. Suponha que os clientes são processos Erlang, que comunicam pelo mecanismo nativo de mensagens, e implemente também as funções de interface apropriadas para serem usadas por estes. 
```erlang
-module(vote).
-export([start/0, vota/0, espera/3, votos/2]).

start() ->
    register (espera, spawn(vote, espera, [0,0,false])),
    register (votos, spawn(vote, votos,[0,0])),
    io:format("criado o processo de votos~n"),
    timer:sleep(2000),
    votos ! {sofia},
    timer:sleep(2000),
    votos ! {tiago},
    timer:sleep(2000),
    votos ! {tiago},
    timer:sleep(2000),
    votos ! {show}.

vota() ->
    receive
        sofia -> 
            votos ! {sofia},
            vota();
        tiago -> 
            votos ! {tiago},
            vota()
end.

votos(S,T)->
    espera ! {S,T},
    receive
        {sofia} ->
            io:format("Voto para a Sofia~n"),
            %espera ! {S+1,T},
            votos(S+1,T);
        {tiago} ->
            io:format("Voto para o Tiago~n"),
            %espera ! {S,T+1},
            votos(S,T+1);
        {show} ->
            io:format("S= ~p, T= ~p ~n",[S,T]),
            votos(S,T)
end.

espera(_,_,true) -> io:format("Tiago ultrapassou a Sofia~n");
espera(_,_,false) ->
    receive
        {S,T} ->
            %io:format("recebi os votos da votacao ~n",[]),
            Res = if
                T > S -> true;
                T < S -> false;
                T == S -> false
            end,
    espera(S,T,Res)
end.
```



