//Observer
trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber
  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber
  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}
trait Subscriber {
  def handler(pub: Publisher)
}
class BankAccount extends Publisher {
  private var balance = 0
  def currentBalance: Int = balance
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
    publish()
  }
  def withdraw(amount: Int): Unit = {
    if (amount > 0 && amount < balance) {
      balance = balance - amount
      publish()
    } else throw new Error("insufficient funds")
  }
}
class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute(): Unit =
    total = observed.map(_.currentBalance).sum
  def handler(pub: Publisher): Unit = compute()
  def totalBalance: Int = total
}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a, b))

c.totalBalance
a deposit 100
c.totalBalance
a withdraw 55
c.totalBalance
b deposit 25
c.totalBalance

//FRP
import scala.util.DynamicVariable

class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  private var observed: List[Signal[_]] = Nil
  update(expr)

  protected def computeValue(): Unit = {
    for (sig <- observed)
      sig.observers -= this
    observed = Nil
    val newValue = caller.withValue(this)(myExpr())
    /* Disable the following "optimization" for the assignment, because we
     * want to be able to track the actual dependency graph in the tests.
     */
    //if (myValue != newValue) {
    myValue = newValue
    val obs = observers
    observers = Set()
    obs.foreach(_.computeValue())
    //}
  }

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    caller.value.observed ::= this
    myValue
  }
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

object Signal {
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}

class BankAccount2 {
  val balance = Var(0)
  def deposit(amount: Int): Unit = {
    val b = balance()
    if (amount > 0) balance() = b + amount
  }
  def withdraw(amount: Int): Unit = {
    if (amount > 0 && amount < balance()) {
      val b = balance()
      if (amount > 0) balance() = b - amount
    } else throw new Error("insufficient funds")
  }
}

def consolidated(accts: List[BankAccount2]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a2 = new BankAccount2
val b2 = new BankAccount2
val c2 = consolidated(List(a2, b2))

c2()
a2 deposit 100
c2()
a2 withdraw 55
c2()
b2 deposit 25
c2()

val exchangeRate = Var(246)
val inDollar = Signal(c2() * exchangeRate())
inDollar()
b2 deposit 100
inDollar()
exchangeRate()=247
inDollar()