import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.*;

class InvalidAccount extends Exception {
    InvalidAccount() { System.out.println("Invalid acount id"); }
}

class NotEnoughFunds extends Exception {
    NotEnoughFunds() { System.out.println("Not enough funds"); }
}

class Bank {
    
    private static class Account {
        Lock l = new ReentrantLock();
        private int balance = 0;
        synchronized int balance() {return balance; }
        synchronized void deposit(int val) {balance += val; }
        synchronized void withdraw (int val) throws NotEnoughFunds {
            if (balance < val) throw new NotEnoughFunds();
            balance -= val;
        } 
    }

    private Map<Integer, Account> acs = new HashMap<>();
    private int nextId = 0;
    private Lock l = new ReentrantLock();
    
    public int createAccount(int bal) {
        Account c = new Account();
        c.deposit(bal);     
        l.lock();
        try {
            acs.put(nextId, c);
            nextId += 1;
            return nextId-1;
        } finally { //garante que o lock é libertado
            l.unlock();
        }
    }

    public int closeAccount(int id) throws InvalidAccount {
        l.lock();
        try {  
            Account c = acs.get(id);
            if (c == null) { throw new InvalidAccount(); }
            acs.remove(id);
            c.l.lock();
        } finally { l.unlock(); }
        try {
            int bal = c.balance();
            return bal;
        } finally { c.l.unlock(); }
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
        withdraw(from, val);
        deposit(to, val);
    }
}

class E2 {

    public static void main(String[] args) throws InvalidAccount, NotEnoughFunds{
        Bank b = new Bank(4);

        for (int i = 0; i < b.accounts.length; i++) {
            System.out.println("Account " + i + " balance is " + b.accounts[i]);
        }
        int[] ids = {0,1,2,3};
        //test methods
        b.deposit(0, 10);
        b.deposit(1, 20);
        b.deposit(2, 30);
        b.deposit(3, 40);
        System.out.println(b.totalBalance(ids));
        b.withdraw(0, 10);
        b.withdraw(1, 10);
        //b.withdraw(2, 40);
        //b.withdraw(4, 10);
        System.out.println(b.totalBalance(ids));
    }
}
