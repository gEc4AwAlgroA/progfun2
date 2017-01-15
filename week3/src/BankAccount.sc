class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Int = {
    if (amount > 0 && amount < balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val a = new BankAccount
a deposit 100
a withdraw 55
//a withdraw 55

val b = new BankAccount
//b withdraw 55


class BankAccountProxy(ba: BankAccount) {
  def deposit(amount: Int): Unit = ba.deposit(amount)
  def withdraw(amount: Int): Unit = ba.withdraw(amount)
}

val c1 = new BankAccount
val c = new BankAccountProxy(c1)

c.deposit(10)
c.withdraw(5)