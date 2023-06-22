# Concurrent Programing
----
## Concurrent Systems

### System Concurrence
- Pseudo Concurrence - Shared CPU time
- True Concurrence - Multiprocessor or Distributed System

### With concurrence processes can:
- execute independent actions
- in simplified models, actions are considered atomic
- run in unpredictable relative velocities
- with shared time actions are interspersed unpredictably
- with true concurrence actions occur simultaneously

### Actions examples:
- read from memory position
- write in memory position
- send message
- receive message

----

## Communication and Synchronization

Various processes might want to cooperate in a common objective.
Two needs verify: communication and synchronization

###  Communication
- passage of information between processes;
- process creates items that are gonna be used by other;
- process server receives requests from clients and returns results;

### Synchronization
- processes can have to wait before continuing to run;
- normally associated to communication;
- wait until item is ready for consumption;
- wait until reply is returned;
- wait until all have finished assigned tasks;
- wait until all components are initialized;
- active undesired waits;

----

No time to see all the material, gonna try to catch the important stuff now 
Pt

### Partilha de memoria vs mensagens
- Memoria partilhada + primitivas de sincronização
- Troca de mensagens

### Mensagens
- Diferentes primitivas:
- - Primitivas básicas tipo send/receive
- - Cliente/Servidor
- - Broadcast/multicast
- - Abstrações de mais alto nível; e.g. comunicação em grupo
- Comunicação síncrona ou assíncrona;
- Comunicação orientada a conexão;
- Formalismos; e.g. CSP - Communication Sequencial Processes 

## Concorrência em memoria partilhada
- As ações dos processos são intercaladas de modo imprevisível;
- Acesso concorrente a dados partilhados gera inconsistência;
- Sao precisas primitivas de controlo de concorrência;
- Primitivas clássicas:
- - Semáforos
- - Monitores
- Monitores são particularmente importantes - onde são baseadas primitivas de muitas linguagens modernas;
- Monitores levam a tipos abstratos de dados concorrentes 

### *Race Conditions: 
Quando processos manipulam concorrentemente uma estrutura de dados partilhada, e o resultado depende da ordem dos acessos.

- Um segmento de código que acede a recursos partilhados é uma secção critica;
- Secções criticas tem de ser submetidas a controlo de concorrência, caso contrario temos race conditions;
- Uma secção critica deve ser rodeada por código de entrada e código de saída;

### Uma solução para o problema das secções criticas deve garantir:
- **Exclusão mutua**: se um processo esta a executar uma secção critica, mais nenhum pode estar;
- **Ausência de deadlock**: se vários processos estão a tentar entrar numa secção critica, um deles deve conseguir;
- **Ausência de starvation**: se um processo tenta entrar numa secção critica, inevitavelmente vai entrar.

Outra formulação para o problema:
- **Exclusão mutua**: se um processo esta a executar uma secção critica, mais nenhum pode estar;
- **Progresso**: se nenhum processo estiver numa secção critica e alguns processos quiserem entrar, apenas os processos que executam o código de sincronização podem participar na decisão de quem entra; esta decisão não pode ser adiada indefinidamente;
- **Espera limitada**: existe um limite para o numero de vezes que outros processos podem entrar passando a frente de um processo que ja pediu entrada.

### Solucao por hardware: Locks
adquirir lock
secção critica
libertar lock


### Exemplo de ordem de execução: produtor-consumidor
Existem 2 tipos de processos:
- produtor: produz itens de dados;
- consumidor: consome itens produzidos.
Itens são produzidos e consumidos para um buffer partilhado de tamanho limitado (bounded buffer).
Uma solução tem de garantir que:
- se o buffer esta vazio, um consumidor não pode prosseguir, tendo que ficar bloqueado;
- se o buffer esta cheio, um produtor não pode prosseguir, tendo que ficar bloqueado.
Este problema também pode envolver exclusão mutua (neste caso na manipulação do buffer).



**Ausência de livelock**: nunca é atingido um de vários possíveis estados dos quais não ha saída (para um estado desejável)

### Relativamente ao controlo de concorrência um objeto pode ser:
- **Sequencial ou atomico**: quando nao permite concorrencia intra-objeto; processa uma invocacao de cada vez.
- **Quase concorrente**: suporta varias invocações em curso, mas no máximo uma não esta suspensa; semelhante ao conceito de monitor.
- **Concorrente**: suporta verdadeiramente concorrência entre invocações, com controlo de concorrência mais fino especificado pelo programador.

### Exemplo de objeto atómico: conta bancaria em Java
```java 
class Conta { 
	private int saldo; 
	
	public synchronized int consulta() { return saldo; } 
	
	public synchronized void deposito(int valor) { saldo = saldo + valor; }
	 
	public synchronized void levantamento(int valor) { saldo = saldo - valor; } 
}
```

### Exemplo: operações sobre objetos em repositórios
```java
Interface Operacao { void aplica(Object o); } 
class Repositorio { 
	public synchronized void insere(String nome, Object o) { 
		// insere o objecto no repositorio 
	} 
	
	public void aplica(String nome, Operacao op) { 
		Object obj; 
		synchronized (this) { 
			obj = ... // procura o objecto pelo nome 
		} 
		synchronized (obj) { 
			op.aplica(obj); // operacao potencialmente demorada 
		} 
	} 
}
```

### Exemplo: operações sobre duas contas bancarias
- Para realizar uma transferência é realizada uma operação de levantamento na primeira conta e outra de deposito na segunda.
```java
c1.levantamento(3000);
c2.deposito(3000);
```
- Suponhamos que é consultado concorrentemente o saldo de cada conta(para por exemplo obter a soma dos saldos).
```java
i = c1.saldo();
j - c2.saldo();
```
- Se tal for efetuado depois do levantamento mas antes do deposito, o resultado é invalido.
- Nestes casos é necessário prevenir interferência entre cada conjunto de operações: obter isolamento.
- Uma hipótese é forcar serialização das operações.
- No caso geral vários clientes podem manipular varias contas.
- Solução: cada cliente adquire os locks dos objetos a manipular, efetua as operações em questão e finalmente liberta os locks.
```java
// cliente 1; realiza uma transferencia 
	c1.lock(); 
	c2.lock(); 
	c1.levantamento(3000); 
	c2.deposito(3000);
	c1.unlock(); 
	c2.unlock(); 
// cliente 2; consulta as duas contas 
	c1.lock(); 
	c2.lock(); 
	i = c1.saldo(); 
	j = c2.saldo(); 
	c1.unlock(); 
	c2.unlock();
```
- Adquirir locks por ordem arbitraria pode causar deadlock.
- Dependências cíclicas de aquisição podem causar deadlock.
- Uma solução para evitar deadlocks é:
- - impor uma ordem total sobre os locks envolvidos;
- - adquirir os locks necessários por ordem, do menor para o maior;
- - (ao libertar a ordem não é importante)

### Two-phase locking
- Cada transação passa por duas fazes, aquisição de locks; libertação de locks.
- Depois de algum lock ser libertado, mais nenhum é adquirido.
- Um lock de um objeto so é libertado quando a transação já possui todos os locks de que necessita.
- Um lock é adquirido o mais tarde possível, na primeira fase;
- Um lock é libertado o mais cedo possível, na segunda fase;
- Operações sobre os objetos em ambas as fases.
```java
// cliente 1; realiza uma transferencia 
	c1.lock(); 
	c1.levantamento(3000); 
	c2.lock(); 
	c1.unlock(); 
	c2.deposito(3000); 
	c2.unlock(); 
// cliente 2; consulta as duas contas 
	c1.lock(); 
	i = c1.saldo(); 
	c2.lock(); 
	c1.unlock(); 
	j = c2.saldo(); 
	c2.unlock();
```

### Exclusão mutua com semáforos
- Semáforo binário iniciado a 1
- Cada processo que quer entrar numa secção critica faz
```java
acquire(S);
//seccao critica
release(S);
```

### Semáforos fortes
```java
acquire(s) {
	if S.v > 0:
		S.v = S.v -1
	else:
		S.l.append(p) //acrescenta no fim da fila
		suspend()
}

release(s) {
	if S.l = []:
		S.v = S.v + 1
	else:
		q = S.l.pop(0) //remove e devolve o primeiro da fila
		ready(q)
}
```


### Exemplo: produtor-consumidor com bounded-buffer
- Dois semáforos: itens e slots.
- Contam os itens no buffer e as posições livres.
```java
// buffer com mutex interno
//Consumidor
while(...) {
	acquire(items);
	x = buffer.take();
	release(slots);
	consume(x);
}

//Produtor
while(...) {
	x = produce();
	acquire(slots);
	buffer.put(x);
	release(items);
}
```

## Acesso direto ao buffer:
- Controlo de concorrência no produtor e consumidor.
- Dois semáforos para exclusão mutua.
- Um para produtores e outro para consumidores.
```java
//Consumidor:
while(...) {
	acquire(itens);
	acquire(mutcons);
	x = buffer[itake];
	itake = (itake + 1) % N;
	release(mutcons);
	release(slots);
	consume(x);
}

//Produtor
while(...) {
	x = produce();
	acquire(slots);
	acquire(mutprod);
	buffer[iput] = x;
	iput = (iput + 1) % N;
	release(mutprod);
	release(itens);
}

```
