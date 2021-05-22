// Peter Idestam-Almquist, 2021-03-07.
// Olivia Jiayi Xu, 2021-03-20.
// Server, multi-threaded, accepting several simultaneous connectedClients.

// package paradis.assignment4;
package Vecka8;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketAddress;
import java.io.PrintWriter;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

class ChatServer implements Runnable {
    private final static int PORT = 8000;
    private final static int MAX_CLIENTS = 5;
    private final static int MAX_MESSAGES = 15;
    private final static Executor executor = Executors.newFixedThreadPool(MAX_CLIENTS);

    private final Socket clientSocket;
    /*
     * ConcurrentHashMap operations don't use locks according to docs.
     * Stores a SocketAddress as key and PrintWriter as value. It stores PrintWriter
     * so that I can print to each client.
     */
    private ConcurrentHashMap<SocketAddress, PrintWriter> connectedClients;
    /*
     * Reference to messageQueue that every Client queues their messages to. This is
     * to ensure that messages are sent in the order the Server receives them.
     */
    private BlockingQueue<String> messageQueue;

    private String clientName = "";

    private ChatServer(Socket clientSocket, ConcurrentHashMap<SocketAddress, PrintWriter> connectedClients,
            BlockingQueue<String> messageQueue) {
        this.clientSocket = clientSocket;
        this.connectedClients = connectedClients;
        this.messageQueue = messageQueue;
    }

    public void run() {
        SocketAddress remoteSocketAddress = clientSocket.getRemoteSocketAddress();
        SocketAddress localSocketAddress = clientSocket.getLocalSocketAddress();
        System.out.println("Accepted client " + remoteSocketAddress + " (" + localSocketAddress + ").");

        BufferedReader socketReader = null;
        try {
            socketReader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

            String threadInfo = " (" + Thread.currentThread().getName() + ").";
            String inputLine = socketReader.readLine();
            System.out.println("Received: \"" + inputLine + "\" from " + remoteSocketAddress + threadInfo);
            clientName = inputLine;                 	// Client name

            while (inputLine != null) {
                inputLine = socketReader.readLine();

                if (inputLine == null)                  // Added condition so that clients can disconnect 
                    break;                              // without the entire server dying

                messageQueue.put(inputLine);            // Append message to the global queue
                String message = inputLine;

                // Peek the shared blocking queue to see if Client's message is the first one,
                // if it is then we send to every client (including itself).
                // ConcurrentHashmap.forEach() sends to every Client in parallel.
                // Retries 1000 times before failing.
                for (int i = 0; i < 1000; i++) {
                    if (messageQueue.peek().equals(inputLine)) {
                        connectedClients.forEach(1, (address, writer) -> {
                            try {
                                writer.println(clientName + ": " + message);
                                System.out.println("Sent: \"" + message + "\" to " + address + " from" + " ("
                                        + Thread.currentThread().getName() + ").");
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                        messageQueue.remove(inputLine); // Removes the message from the global queue
                        break;
                    }
                    try {
                        Thread.sleep(10);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
                System.out.println(
                        "Received: \"" + inputLine + "\" from " + clientName + " " + remoteSocketAddress + threadInfo);
            }
            System.out.println("Closing connection " + remoteSocketAddress + " (" + localSocketAddress + ").");
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (socketReader != null)
                    socketReader.close();
                if (clientSocket != null)
                    clientSocket.close();
                connectedClients.remove(remoteSocketAddress);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("Server started.");

        // Server and client sockets
        ServerSocket serverSocket = null;
        Socket clientSocket = null;
        ConcurrentHashMap<SocketAddress, PrintWriter> connectedClients = new ConcurrentHashMap<>();
        BlockingQueue<String> messageQueue = new ArrayBlockingQueue<String>(MAX_MESSAGES);

        try {
            serverSocket = new ServerSocket(PORT);
            SocketAddress serverSocketAddress = serverSocket.getLocalSocketAddress();
            System.out.println("Listening (" + serverSocketAddress + ").");

            while (true) {
                clientSocket = serverSocket.accept();
                connectedClients.put(clientSocket.getRemoteSocketAddress(),
                        new PrintWriter(clientSocket.getOutputStream(), true));
                executor.execute(new ChatServer(clientSocket, connectedClients, messageQueue));
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (serverSocket != null)
                    serverSocket.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
