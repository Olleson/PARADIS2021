// Peter Idestam-Almquist, 2020-02-04.
// Olivia Jiayi Xu, 2021-03-09.
/*
Parallelized program that distributes work in a producer-consumer pattern with a common forkjoinpool thread pool.
Thread safe because it uses blocking queue synchronization - blocks until item is obtainable in queue.
When there is an available object to be processed, submit it to the pool.

Upon initialization, place webpages to download_queue ->
When download is called, empty the queue and for each webpage that is in there, submit it to the thread pool ->
When analyze is called, empty the queue and for each webpage that is in there, submit it to the thread pool ->
When categorize is called, empty the queue and for each webpage that is in there, submit it to the thread pool ->
*/

// package paradis.assignment3;
package Vecka7;

// [You are welcome to add some import statements.]
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ForkJoinPool;

public class Program1 {
    final static int NUM_WEBPAGES = 100;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    private static ForkJoinPool threadPool = ForkJoinPool.commonPool();

    private static BlockingQueue<WebPage> download_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> analyze_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> categorize_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> result_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    // Made it add the pages to the download_queue.
    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
            try {
                download_queue.put(webPages[i]);
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void downloadWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage page = download_queue.take();
                threadPool.submit(() -> {
                    try {
                        page.download();
                        analyze_queue.put(page);
                    } catch (Exception e) {
                        System.out.println(e);
                    }
                });
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void analyzeWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage page = analyze_queue.take();
                threadPool.submit(() -> {
                    try {
                        page.analyze();
                        categorize_queue.put(page);
                    } catch (Exception e) {
                        System.out.println(e);
                    }
                });
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void categorizeWebPages() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage page = categorize_queue.take();
                threadPool.submit(() -> {
                    try {
                        page.categorize();
                        result_queue.put(page);
                    } catch (Exception e) {
                        System.out.println(e);
                    }
                });
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    // Changed it so that it collects the results before printing it all out.
    // The printout got mixed up with the WebPage print outs otherwise and it bothered me.
    private static void presentResult() {
        WebPage[] results = new WebPage[NUM_WEBPAGES];
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            try {
                WebPage page = result_queue.take();
                results[i] = page;
            } catch (Exception e) {
                System.out.println(e);
            }
        }
        for (WebPage p : results)
            System.out.println(p);
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        // Do the work.
        downloadWebPages();
        analyzeWebPages();
        categorizeWebPages();

        // Stop timing.
        long stop = System.nanoTime();

        // Present the result.
        presentResult();

        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}