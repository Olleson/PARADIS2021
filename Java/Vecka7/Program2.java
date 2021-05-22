// Peter Idestam-Almquist, 2020-02-04.
// Olivia Jiayi Xu, 2021-03-09.
/*
Parallelized program that distributes work in a producer-consumer pattern with a parallel stream thread pool.
*/

// [Do necessary modifications of this file.]

//package paradis.assignment3;
package Vecka7;

import java.util.Arrays;

public class Program2 {
    final static int NUM_WEBPAGES = 100;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
        }
    }

    // [Do modify this sequential part of the program.]
    private static WebPage downloadWebPage(WebPage page) {
        page.download();
        return page;
    }

    // [Do modify this sequential part of the program.]
    private static WebPage analyzeWebPage(WebPage page) {
        page.analyze();
        return page;
    }

    // [Do modify this sequential part of the program.]
    private static WebPage categorizeWebPage(WebPage page) {
        page.categorize();
        return page;
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    // Makes it take in an array of WebPages, excessive but, I didn't like how the printout look with a forEach at the end.
    private static void presentResult(WebPage[] pages) {
        for (WebPage p : pages) {
            System.out.println(p);
        }
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        // Do the work.
        WebPage[] done = Arrays.asList(webPages)
            .parallelStream()
            .map(Program2::downloadWebPage)
            .map(Program2::analyzeWebPage)
            .map(Program2::categorizeWebPage)
            .toArray(WebPage[]::new);

        // Stop timing.
        long stop = System.nanoTime();

        // Present the result.
        presentResult(done);

        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}

// alt 1
// No need to presentResult()
// Arrays.asList(webPages)
// .parallelStream()
// .map(Program2::downloadWebPage)
// .map(Program2::analyzeWebPage)
// .map(Program2::categorizeWebPage)
// .forEach(Program2::presentResult);

// alt 2
// Each method doesnt return anything
// Arrays.asList(webPages)
// .parallelStream()
// .forEach(s -> {
// downloadWebPage(s);
// analyzeWebPage(s);
// categorizeWebPage(s);
// });
