// Olivia Aixinjuelo Xu, 2021-02-23, B-grade
/*
Prime Factorizer with a shared State-class. State-class has a lock and a completion boolean.
Each thread synchronizes with its lock, meaning that only one thread can update Factor1 and Factor2 if
it ever finds the factors. However, the outer loop reads the State class's state, therefore 
this implementation is not thread safe. (The reading is unprotected and unsynchronized).
*/

// package paradis.assignment1;
package Vecka5;
import java.math.BigInteger;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Factorizer implements Runnable {
    private final BigInteger product;
    private final BigInteger min;
    private final BigInteger max;
    private final BigInteger step;
    private State state;
    private BigInteger factor1;
    private BigInteger factor2;

    private static class State {
        private boolean completed = false;
        private Object lock = new Object();
    }

    private Factorizer(BigInteger product, BigInteger max, int min, int step, State state) {
        this.product = product;
        this.max = max;
        this.min = BigInteger.valueOf(min);
        this.step = BigInteger.valueOf(step);
        this.state = state;
    }

    public void run() {
        BigInteger number = min;
        while (number.compareTo(max) == -1 && !state.completed) {   // Does not check for max number and stops right before unlike original code
            if (product.remainder(number).compareTo(BigInteger.ZERO) == 0) {
                synchronized(state.lock) {
                    if (state.completed) { return; }        // If completed when factors are found, return because we only want the result of the first factorizer that found answer
                    factor1 = number;
                    factor2 = product.divide(factor1);
                    state.completed = true;
                    //System.out.println("Factors: [" + factor1 + "," + factor2 + "]");
                    return;
                }
            }
            number = number.add(step);
        }
    }

    public static void main(String[] args) {
        InputStreamReader streamReader = new InputStreamReader(System.in);
        BufferedReader inputReader = new BufferedReader(streamReader);
        try {
            // Inputs
            System.out.print("Input product> ");
            BigInteger product = new BigInteger(inputReader.readLine());
            System.out.print("Input number of threads> ");
            int numThreads = Integer.parseInt(inputReader.readLine());

            // Make threads
            long start = System.nanoTime();
            State state = new State();
            Thread[] threads = new Thread[numThreads];
            Factorizer[] factorizers = new Factorizer[numThreads];
            for (int i = 0; i < numThreads; i++) {
                factorizers[i] = new Factorizer(product, product, 2 + i, numThreads, state);
                threads[i] = new Thread(factorizers[i]);
            }

            // Start, join and collect
            for (Thread t : threads) { t.start(); }
            // for (Thread t : threads) { t.join(); }
            for (int i = 0; i < numThreads; i++) { 
                threads[i].join();
                if (factorizers[i].factor1 != null && factorizers[i].factor2 != null) {
                    System.out.println("Factors: [" + factorizers[i].factor1 + "," + 
                                        factorizers[i].factor2 + "]");
                }
            }
            if (!state.completed) { System.out.println("Factorization is not possible"); }

            long stop = System.nanoTime();
            System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);

        } catch (Exception e) {
            System.out.println(e);
        }
    }
}