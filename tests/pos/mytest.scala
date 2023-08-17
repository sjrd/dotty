class Parent[T](val x: T)

class Child[T](x: T) extends Parent(x)

type MT[X] = X match
  case Parent[Int] => Int
  case Parent[Boolean] => Boolean

def test(): Unit =
  summon[MT[Parent[Int]] =:= Int]
  summon[MT[Parent[Boolean]] =:= Boolean]
  //summon[MT[Parent[Any]] =:= Int]

  summon[MT[Child[Int]] =:= Int]
  summon[MT[Child[Boolean]] =:= Boolean]
end test
