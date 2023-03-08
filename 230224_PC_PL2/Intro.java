import java.lang.Runnable;
import java.lang.Thread; 

//Declare a subclass of Thread, should override the run method of class 
//Thread, an instance of the subclass can then be allocated and started.
class MyThread extends Thread {
    public void run() {
        for(int i = 0; i < 10; i++) {
            System.out.println("Print na thread");
        }
    }
}
//Declare a class that implements the Runnable interface then implement
//the run method. An instance of the class can then be allocated, passed
//as an argument when creating Thread and started.
class MyRunnable implements Runnable {
    public void run() {
        try {
            Thread.sleep(1000);
            System.out.println("Print na thread 2");
        }
        catch (InterruptedException ignored) {}
    }
}

class Intro {
    public static void main(String[] args) throws InterruptedException {
        MyThread t = new MyThread();
        //t.run(); <--- Nunca Fazer
        t.start();

        Thread t2 = new Thread(new MyRunnable());
        System.out.println("Print no Main");
        t2.start(); 
        t.join();
        t2.join();

        System.out.println("Print no fim do Main");
    }
}
