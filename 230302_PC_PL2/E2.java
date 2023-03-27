import java.util.Random;
import java.lang.Thread;
class InvalidAccount extends Exception {
    InvalidAccount() { System.out.println("Invalid acount id"); }
}

class NotEnoughFunds extends Exception {
    NotEnoughFunds() { System.out.println("Not enough funds"); }
}

class Bank {

    private static class Account {
        private int balance = 0;
        synchronized int balance() {return balance; }
        synchronized void deposit(int val) {balance += val; }
        synchronized void withdraw (int val) throws NotEnoughFunds {
            if (balance < val) throw new NotEnoughFunds();
        } 
    }

    private Account[] acs;

    public Bank(int N) {
        acs = new Account[N];
        for (int i = 0; i < N; i++) acs[i] = new Account();
    }

    
    public void deposit(int id, int val) throws InvalidAccount {
        if (id < 0 || id >= acs.length) { throw new InvalidAccount(); }
        Account c = acs[id];
        c.deposit(val);
    }

    public void withdraw(int id, int val) throws InvalidAccount, NotEnoughFunds {
        if (id < 0 || id >= acs.length) { throw new InvalidAccount(); }
        Account c = acs[id];
        c.withdraw(val);
    }
    
    public int balance(int id) throws InvalidAccount {
        if(id < 0 || id >= acs.length) { throw new InvalidAccount(); }
        Account c = acs[id];
        return c.balance();
    }

    int totalBalance(int accounts[]) throws InvalidAccount {
        int res = 0;
        for (int id : accounts) {
            res += balance(id); 
        }
        return res;
    }

    public void transfer(int from, int to, int val) throws InvalidAccount, NotEnoughFunds {
        if (from < 0 || from >= acs.length || to < 0 || to >= acs.length) throw new InvalidAccount();
        Account acfrom = acs[from];
        Account acto = acs[to];
        Account c1, c2;
        if (from < to) {
            c1 = acfrom;
            c2 = acto;
        } else {
            c2 = acfrom;
            c1 = acto;
        }
        synchronized (c1) {
            synchronized (c2) {
                acfrom.withdraw(val);
                acto.deposit(val);
            }
        }
    }
}
class E2 {
    public static void main(String[] args) throws Exception {
        final int c = Integer.parseInt(args[0]);
        final int v = Integer.parseInt(args[1]);
        Bank b = new Bank(c);

        for(int i = 0; i < c; ++i) { b.deposit(i,v); }

        int[] ids = new int[c];
        for(int i = 0; i < c; ++i) { ids[i] = i; }

        new Thread(() ->{
            Random r = new Random();
            try{
                while(true) {
                    int from = r.nextInt(c);
                    int to = r.nextInt(c);
                    b.transfer(from,to,100);
                }
            }catch (Exception e){
                System.out.println("fim da thread de tranferencia");
            }
        }).start();

        new Thread(() -> {
            try{
                while (true){
                    int total = b.totalBalance(ids);
                    if(total != c * v) System.out.println("balance total errado: " + total);
                    java.lang.Thread.sleep(100); 
                }
            }catch (Exception e) {
                System.out.println("fim da thread print total");
            }
        }).start();
    }
}

