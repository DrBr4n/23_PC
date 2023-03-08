class Counter {
    private int value = 0;
    
    void increment() { value += 1; }

    int value() { return value;}
}

class Incrementer extends Thread {
    final int I;
    final Counter c;

    Incrementer(int I, Counter c) { this.I = I; this.c = c; }

    public void run() {
        for(int i = 1; i <= I; i++) {
            c.increment();
        }
    }
}

class E2v2 {
    public static void main(String[] args) throws InterruptedException {
        final int N = Integer.parseInt(args[0]);
        final int I = Integer.parseInt(args[1]);
        
        Counter c = new Counter();
        
        Thread[] a = new Thread[N];
        for(int i = 0; i < N; ++i) a[i] = new Incrementer(I, c);
        for(int i = 0; i < N; ++i) a[i].start();
        for(int i = 0; i < N; ++i) a[i].join();
    
        System.out.println(c.value());
    }
}
