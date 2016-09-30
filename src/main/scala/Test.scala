@memoized
object Test extends App {
  def calculate1(a: Int): Int = {
    println("executing1...")
    a
  }


  println(calculate1(1))
  println(calculate1(1))
  println(calculate1(2))

  def calculate2(a: Int): Int = {
    println("executing2...")
    a
  }

  println(calculate2(1))
  println(calculate2(1))
  println(calculate2(2))
  println(calculate2(2))
}
