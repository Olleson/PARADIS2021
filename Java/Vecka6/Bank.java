// Olivia Aixinjuelo, 2021-03-05.

/*
Thread safety with ReentrantReadWriteLocks. Reads are managed with Read lock, Writes are managed with Write lock on a per account basis.
Each account has a corresponding lock in another arraylist. Implements a sleep-retry mechanic for each lock. Reentrant is also used
to allow for nested locks - so, writelock.lock -> writelock.lock -> writelock.unlock -> writelock.unlock. Users may attempt to
regain the same lock in this implementation multiple times. Without reetrant, it would not be able to re-acquire it.

*EDIT: Usage of items as locks from array list not thread safe - check java docs to see what's thread safe or not (there might be something inside the array list that does things in unsafe ways, idk bro)
       A better solution would be to use lock ordering that's not reliant on sleep-retry.
*/

//package paradis.assignment2;
package Java.Vecka6;

import java.util.List;
import java.util.Random;
import java.util.ArrayList;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class Bank {
	/*
	 * I thought the original data structure for accounts was fine and
	 * used another list for lock management. Because I hadn't
	 * changed how ids are made, locks and accounts will always be equivalent:
	 * accounts[0] == locks[0]. They're not dependent on each other so there are
	 * some pitfalls with this approach, and if not properly maintained, can be
	 * inconsistent.
	 * 
	 * ReentrantReadWriteLocks seemed appropriate too as they allow for separate
	 * locks for reading and writing. It might be overkill for an application like
	 * this (gets written more than read) though.
	 */
	private final List<Account> accounts = new ArrayList<Account>();
	private final List<ReentrantReadWriteLock> locks = new ArrayList<ReentrantReadWriteLock>();
	private final Random random = new Random();	// Random instance to use for sleeps

	/*
	 * Synchronized method so that identical accounts can't be made at the
	 * same time. Not sure what you mean by FIX ORIGINAL, and nothing was written in
	 * the assignment description about it, so I didn't change it.
	 */
	int newAccount(int balance) {
		synchronized (this) {
			int accountId = accounts.size(); // FIX ORIGINAL
			accounts.add(new Account(accountId, balance));
			locks.add(new ReentrantReadWriteLock(true));
			return accountId;
		}
	}

	/*
	 * Applied a sleep-retry method for the reading of balances. Allows multiple
	 * threads to read and only fails if it's being modified/written by some other
	 * Thread. To prevent it from being blocked, it retries if it fails. Returns -1
	 * if it fails to acquire the lock 100 times.
	 */
	int getAccountBalance(int accountId) {
		ReentrantReadWriteLock lock = locks.get(accountId);
		Account account = accounts.get(accountId);
		for (int i = 0; i < 100; i++) {
			if (lock.readLock().tryLock()) {
				try {
					return account.getBalance();
				} finally {
					lock.readLock().unlock();
				}
			}
			try {
				Thread.sleep(random.nextInt(200));
			} catch (InterruptedException e) {
				System.out.println(e);
			}
		}
		System.out.println(Thread.currentThread().getName() + "failed to get readLock");
		return -1;
	}

	/*
	 * Same thing as the above method, sleep-retry if it fails. Does not do the
	 * deposit if it fails 100 times.
	 */
	void runOperation(Operation operation) {
		int id = operation.getAccountId();
		Account account = accounts.get(id);
		ReadWriteLock lock = locks.get(id);
		for (int i = 0; i < 100; i++) {
			if (lock.writeLock().tryLock()) {
				try {
					int balance = account.getBalance();
					balance = balance + operation.getAmount();
					account.setBalance(balance);
					return;
				} finally {
					lock.writeLock().unlock();
				}
			}
			try {
				Thread.sleep(random.nextInt(200));
			} catch (InterruptedException e) {
				System.out.println(e);
			}
		}
	}

	/*
	 * Helper-method that unlocks all the locks in a given list (if the
	 * thread is holding its lock).
	 */
	void unlockLocks(List<Operation> ops) {
		for (Operation op : ops) {
			if (locks.get(op.getAccountId()).writeLock().isHeldByCurrentThread()) {
				locks.get(op.getAccountId()).writeLock().unlock();
			}
		}
	}

	/*
	 * Beefy boy method. Helper-method used in runTransaction to take all the
	 * required locks for a Transaction's operations. It returns true if successful
	 * and returns false if it fails. This should only happen if there is
	 * something wrong with the code.
	 * 
	 * When it fails to acquire a lock, it unlocks everything it has locked up until
	 * that point. Then it breaks out of operations-loop and retries
	 * from the first operation after sleeping.
	 */
	boolean takeLocks(List<Operation> operations) {
		List<Operation> locked = new ArrayList<Operation>();
		for (int i = 0; i < 100; i++) {
			for (Operation op : operations) {
				if (!locks.get(op.getAccountId()).writeLock().tryLock()) {
					// System.out.println(Thread.currentThread().getName() + " failed, unlocking
					// acquired locks");
					unlockLocks(locked);
					break;
				}
				locked.add(op);
				if (locked.size() == operations.size()) {
					// System.out.println(Thread.currentThread().getName() + " succeeded");
					return true;
				}
			}
			try {
				// System.out.println(Thread.currentThread().getName() + " retrying");
				Thread.sleep(random.nextInt(200));
			} catch (InterruptedException e) {
				System.out.println(e);
			}
		}
		// System.out.println(Thread.currentThread().getName() + " failed");
		return false;
	}

	/*
	 * Calls the takeLocks method and if it failed, it cancels the Transaction
	 * entirely. Like I mentioned earlier, should never happen but in the rare case
	 * it does, the Transaction aborts and does not get completed.
	 * 
	 * After running all operations, it unlocks all of the locks held by a Thread
	 * with unlockLocks.
	 */
	void runTransaction(Transaction transaction) {
		List<Operation> currentOperations = transaction.getOperations();
		if (!takeLocks(currentOperations)) {
			return;
		}
		for (Operation operation : currentOperations) {
			runOperation(operation);
		}
		unlockLocks(currentOperations);
	}
}
