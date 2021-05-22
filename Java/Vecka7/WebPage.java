// Peter Idestam-Almquist, 2020-02-04.

// [Do NOT modify this file.]
// package paradis.assignment3;
package Vecka7;

import java.util.Random;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;
import java.util.HashMap;

@SuppressWarnings("unused")	// I added
public class WebPage {
	private final int id;
	private URI uri;
	private String content = null;
	private Map<String, Integer> wordMap = new HashMap<String, Integer>();
	private int maxCounter = 0;
	private String category = null;

	private static long sum = 0; // Only used to fake hard work.

	WebPage(int id, String uriString) {
		this.id = id;
		try {
			uri = new URI(uriString);
		} catch (URISyntaxException exception) {
		}
	}

	/**
	 * Performs a fake download of text from a webpage (it just generates a string
	 * of nonsense words).
	 */
	void download() {
		Random random = new Random();
		int numWords = 1 + random.nextInt(10);
		String content = "";
		for (int i = 0; i < numWords; i++)
			content += "word" + (random.nextInt(5) + 1) + " ";
		this.content = content;
		// Fake wait for web-response.
		try {
			Thread.sleep(200);
		} catch (InterruptedException exception) {
			System.out.println(exception);
		}
		System.out.println("Downloaded: " + id);
	}

	/**
	 * Count inputted word.
	 * 
	 * @param word Count word.
	 */
	private void countWord(String word) {
		if (wordMap.containsKey(word))
			wordMap.put(word, wordMap.get(word) + 1);
		else
			wordMap.put(word, 1);
	}

	/**
	 * Counts occurrences of all different words occurring in the downloaded text.
	 */
	void analyze() {
		String[] words = content.split(" ");
		for (int i = 0; i < words.length; i++)
			countWord(words[i]);
		// Do some hard work.
		sum = 0;
		for (int i = 0; i < Integer.MAX_VALUE / 10; i++) {
			sum += i;
		}
		System.out.println("Analyzed: " + id);
	}

	/**
	 * Updates maxCounter and updates category for the website if counter is greater
	 * than current maxCounter.
	 * 
	 * @param word A word.
	 * @param counter Inputted occurences of word to check.
	 */
	private void storeMax(String word, int counter) {
		if (counter > maxCounter) {
			maxCounter = counter;
			category = word;
		}
	}

	/**
	 * Performs a categorization of the text based on the word with most occurrences
	 * in the text.
	 */
	void categorize() {
		wordMap.forEach((k, v) -> storeMax(k, v));
		// Do some hard work.
		sum = 0;
		for (int i = 0; i < Integer.MAX_VALUE / 20; i++) {
			sum += i;
		}
		System.out.println("Categorized: " + id);
	}

	/**
	 * Returns a string representation of the categorized webpage.
	 * 
	 * @return String of categorized webpage.
	 */
	public String toString() {
		String tmp = "";
		if (uri != null)
			tmp += "uri = " + uri;
		if (category != null)
			tmp += "; category = " + category;
		return tmp;
	}
}
