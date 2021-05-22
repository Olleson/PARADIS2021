// Peter Idestam-Almquist, 2020-01-31.

// package paradis.assignment2;
package Vecka6;

class Account {
	// Instance variables.
	private final int id;
	private int balance;
	
	// Constructor.
	Account(int id, int balance) {
		this.id = id;
		this.balance = balance;
	}
	
	// Instance methods.
	int getId() {
		return id;
	}
	
	int getBalance() {
		return balance;
	}
	
	void setBalance(int balance) {
		this.balance = balance;
	}
}
