// Incomplete

// Peter Idestam-Almquist, 2020-02-04.
// [Replace this comment with your own name.]

// [Do necessary modifications of this file.]

//package paradis.assignment3;
package Vecka7;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

// [You are welcome to add some import statements.]

public class Program3 {
    final static int NUM_WEBPAGES = 40;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    private static MyExecutor executor = new MyExecutor();

    private static BlockingQueue<WebPage> download_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> analyze_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> categorize_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);
    private static BlockingQueue<WebPage> result_queue = new ArrayBlockingQueue<WebPage>(NUM_WEBPAGES);

    // [You are welcome to modify this method, but it should NOT be parallelized.]
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
                executor.submit(() -> {
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
                executor.submit(() -> {
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
                executor.submit(() -> {
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

    private static class MyExecutor implements ExecutorService {

        public MyExecutor() {

        }

        @Override
        public void execute(Runnable arg0) {
            // TODO Auto-generated method stub

        }

        @Override
        public <T> Future<T> submit(Callable<T> arg0) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public Future<?> submit(Runnable arg0) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public <T> Future<T> submit(Runnable arg0, T arg1) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public boolean awaitTermination(long arg0, TimeUnit arg1) throws InterruptedException {
            // TODO Auto-generated method stub
            return false;
        }

        @Override
        public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> arg0) throws InterruptedException {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> arg0, long arg1, TimeUnit arg2)
                throws InterruptedException {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public <T> T invokeAny(Collection<? extends Callable<T>> arg0) throws InterruptedException, ExecutionException {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public <T> T invokeAny(Collection<? extends Callable<T>> arg0, long arg1, TimeUnit arg2)
                throws InterruptedException, ExecutionException, TimeoutException {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public boolean isShutdown() {
            // TODO Auto-generated method stub
            return false;
        }

        @Override
        public boolean isTerminated() {
            // TODO Auto-generated method stub
            return false;
        }

        @Override
        public void shutdown() {
            // TODO Auto-generated method stub

        }

        @Override
        public List<Runnable> shutdownNow() {
            // TODO Auto-generated method stub
            return null;
        }

    }

}
