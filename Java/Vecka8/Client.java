// Peter Idestam-Almquist, 2021-03-07.
// Client, two-threaded, responsive to incoming messages.

//package paradis.assignment4;
package Vecka8;

import java.net.Socket;
import java.net.SocketAddress;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;

class Client implements Runnable {
	private final static String HOST = "127.0.0.1";
	private final static int PORT = 8000;

	private final static int NUM_TEST_MESSAGES = 10000000;

	private final BufferedReader socketReader;

	private Client(BufferedReader socketReader) {
		this.socketReader = socketReader;
	}

	// Receive messages.
	public void run() {
		try {
			String threadInfo = " (" + Thread.currentThread().getName() + ").";
			while (true) {
				if (socketReader.ready()) {
					String line = socketReader.readLine();
					System.out.println("Received: \"" + line + "\"" + threadInfo);
					// System.out.println(line + threadInfo);
				}
				Thread.sleep(100);
			}
		} catch (InterruptedException exception) {
		} catch (Exception exception) {
			System.out.println(exception);
		}
	}

	public static void main(String[] args) {
		System.out.println("Client started.");

		Socket socket = null;
		PrintWriter socketWriter = null;
		BufferedReader socketReader = null;
		BufferedReader consoleReader = null;
		try {
			socket = new Socket(HOST, PORT);
			SocketAddress remoteSocketAddress = socket.getRemoteSocketAddress();
			SocketAddress localSocketAddress = socket.getLocalSocketAddress();
			System.out.println("Connected to server " + remoteSocketAddress + " (" + localSocketAddress + ").");

			socketWriter = new PrintWriter(socket.getOutputStream(), true);
			socketReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

			// Run socketReader in separate thread from consoleReader.
			Thread thread = new Thread(new Client(socketReader));
			thread.start();

			// Send messages.
			String threadInfo = " (" + Thread.currentThread().getName() + ").";
			consoleReader = new BufferedReader(new InputStreamReader(System.in));
			String message = consoleReader.readLine();

			String[] testMessages = null;
			while (message != null && !message.equals("close")) {
				if (message != null && !message.isEmpty()) {
					socketWriter.println(message);
					System.out.println("Sent: \"" + message + "\"" + threadInfo);
				}
				if (message != null && !message.isEmpty() && message.equals("test")) {
					testMessages = test();
					for (String m : testMessages) {
						socketWriter.println(m);
						System.out.println("Sent: \"" + m + "\"" + threadInfo);
						if (m.equals(String.valueOf(NUM_TEST_MESSAGES-1))){
							System.out.println("Finished sending messages");
						}
					}
				}
				message = consoleReader.readLine();
				// if (message != null && !message.isEmpty()) {
				// socketWriter.println(message);
				// System.out.println("Sent: \"" + message + "\"" + threadInfo);
				// }
				// message = consoleReader.readLine();
			}

			thread.interrupt();
			thread.join();
			System.out.println("Closing connection " + remoteSocketAddress + " (" + localSocketAddress + ").");
		} catch (Exception exception) {
			System.out.println(exception);
		} finally {
			try {
				if (socketWriter != null)
					socketWriter.close();
				if (socketReader != null)
					socketReader.close();
				if (socket != null)
					socket.close();
				if (consoleReader != null)
					consoleReader.close();
			} catch (Exception exception) {
				System.out.println(exception);
			}
		}
	}

	public static String[] test() {
		String[] testMessages = new String[NUM_TEST_MESSAGES];
		for (int i = 0; i < NUM_TEST_MESSAGES; i++) {
			testMessages[i] = (i + ".");
		}
		return testMessages;
	}
}
