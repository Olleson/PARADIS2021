package Java.Vecka6;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Random;

public class BankTest {
    static boolean running = true;
    static InputStreamReader streamReader = new InputStreamReader(System.in);
    static BufferedReader inputReader = new BufferedReader(streamReader);

    public static void main(String[] args) throws NumberFormatException, IOException {
        while (running) {
            System.out.print("Input test1 or test2 or operation> ");
            manageInput(inputReader.readLine());
        }
    }

    public static void manageInput(String input) {
        BankTest bankTest = new BankTest();
        switch (input) {
            case "test1":
                System.out.println("Transaction test but of one account");
                bankTest.test1();
                break;
            case "test2":
                System.out.println("Tests transaction with modifiable variables");
                bankTest.test2();
                break;
            case "operation":
                System.out.println("Simple operation test - does not test Transaction");
                bankTest.testOperation();
                break;
            case "stop":
            default:
                running = false;
        }
    }

    private void testOperation() {
        try {
            System.out.println("10 operations in 10 threads adding to one account by 1.");

            // Instance variables
            Bank bank = new Bank();
            int id = bank.newAccount(0);
            Thread[] threads = new Thread[10];
            int expected = 0;

            for (int i = 0; i < 10; i++) {
                expected += i;
                threads[i] = new Thread(new Operation(bank, id, i));
            }
            for (int i = 0; i < 10; i++) {
                threads[i].start();
            }
            for (int i = 0; i < 10; i++) {
                threads[i].join();
            }

            System.out.println("Expected: " + expected);
            System.out.println("Actual: " + bank.getAccountBalance(id));
        } catch (Exception e) {
            System.out.println(e);
        }
    }
    private void test1() {
        try {
            System.out.println(
                    "Testing 1000 threads, with each thread having 100 operations that append by 1 of the same account.");
            Thread.sleep(3000);

            Bank bank = new Bank();
            int id = bank.newAccount(0);

            Thread[] threads = new Thread[1000];
            Operation operation = new Operation(bank, id, 1);
            Transaction transaction = new Transaction(bank);

            long start = System.nanoTime();

            for (int i = 0; i < 100; i++) {
                transaction.add(operation); // each transaction adding 1000, which is done 1000 times... 1000 * 1000 = 1
                                            // 000 000
            }
            try {
                for (int i = 0; i < 1000; i++) {
                    threads[i] = new Thread(transaction);
                }
                for (int i = 0; i < 1000; i++) {
                    threads[i].start();
                }
                for (int i = 0; i < 1000; i++) {
                    threads[i].join();
                }
            } catch (Exception e) {
                throw e;
            }
            System.out.println("Balance: " + bank.getAccountBalance(id));
            System.out.println("Execution time (seconds): " + (System.nanoTime() - start) / 1.0E9);
            System.out.println("Expected balance: " + 100000);
        } catch (Exception e) {
            System.out.println(e);
        }
        return;
    }
    public void test2() {
        try {
            // Testing values
            int numAccounts = 30;
            int numOperations = 50;
            int numTransactions = 10;
            int numThreads = 10;
            int balance = 0;
            int deposit = 1;

            // Instance variables
            Random rnd = new Random();
            Bank bank = new Bank();
            int[] accounts = new int[numAccounts];
            Thread[] threads = new Thread[numThreads];
            Operation[] operations = new Operation[numOperations];
            Transaction[] transactions = new Transaction[numTransactions];
            int[] expected_results = new int[numAccounts];

            // Message to user
            System.out.println("Testing with [" + numAccounts + " accounts, " + numOperations + " operations, "
                    + numTransactions + " transactions, " + numThreads + " threads]. \nEach account has " + balance
                    + " balance and gets deposited " + deposit + " per operation per transaction.");
            // Pauses just to let person read the test variables
            Thread.sleep(3000);

            // Make accounts, also fill expected_results
            for (int i = 0; i < numAccounts; i++) {
                accounts[i] = bank.newAccount(balance);
                expected_results[i] = balance;
            }

            // Random account chosen to deposit to
            for (int i = 0; i < numOperations; i++) {
                int acc = rnd.nextInt(numAccounts);
                // System.out.println("depositing " + deposit + " to account " + acc);
                operations[i] = new Operation(bank, acc, deposit);
                expected_results[acc] += deposit;
            }

            // Print expected results
            System.out.println("Expected:");
            for (int i = 0; i < numAccounts; i++) {
                expected_results[i] *= numThreads;
                System.out.print("[id: " + i + ", " + expected_results[i] + "], ");
            }

            // Make numTransactions amount of transactions, each with operations amount of
            // operations to do
            for (int i = 0; i < numTransactions; i++) {
                transactions[i] = new Transaction(bank);
                for (Operation o : operations) {
                    transactions[i].add(o);
                }
            }

            // Start timer
            long start = System.nanoTime();
            System.out.println("\nStarting timer...");

            // Distribute all transactions among all threads, so Thread A can get another Transaction
            int z = 0;
            for (int x = 0; x < numThreads; x++) {
                if (z < numTransactions) {
                    if (z == numTransactions - 1) {
                        z = 0;
                    }
                    threads[x] = new Thread(transactions[z]);
                }
                z++;
            }

            // Start and join threads
            System.out.println("Starting threads...");
            for (int i = 0; i < numThreads; i++) {
                threads[i].start();
            }
            System.out.println("Joining threads...");
            for (int i = 0; i < numThreads; i++) {
                threads[i].join();
            }

            // End timer
            double length = (System.nanoTime() - start) / 1.0E9;

            // Print actual results
            System.out.println("Actual:");
            int[] results = new int[numAccounts];
            for (int i = 0; i < numAccounts; i++) {
                results[i] = bank.getAccountBalance(i);
                System.out.print("[id: " + i + ", " + bank.getAccountBalance(i) + "], ");
            }

            // List matching and execution time
            System.out.println("\nLists are " + (Arrays.equals(expected_results, results) ? "matching!" : "not matching..."));
            System.out.println("Execution time (seconds): " + length);
        } catch (Exception E) {
            System.out.println(E);
        }
    }
}
