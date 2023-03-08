class Printer extends Thread {
    final int I;
    final int n;

    Printer(int I, int n) {
        this.I = I; 
        this.n = n; 
    }

    public void run() {
        for(int i = 1; i < I; i++) {
            System.out.println("Thread " + n + " escreve " + i);
        }
    }
}

class E1 {
    public static void main(String[] args) throws InterruptedException {
        final int N = Integer.parseInt(args[0]);
        final int I = Integer.parseInt(args[1]);
    
        //Thread[] a = new Thread[N];
        Printer[] a = new Printer[N];
        for(int i = 0; i < N; ++i) a[i] = new Printer(I, i+1);
        for(int i = 0; i < N; ++i) a[i].start();
        for(int i = 0; i < N; ++i) a[i].join();
    }
}
