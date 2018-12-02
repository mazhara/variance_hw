package variance.hw

trait ContrVOrdering[-T] {
  def compare(a: T, b: T): Int
}

object ContrVOrdering {

  implicit val stringCompare: ContrVOrdering[String] = new ContrVOrdering[String] {
    override def compare(a: String, b: String): Int = {
      println(s"a = ${a}, b = ${b}")
      a.compare(b)
    }
  }

  implicit val mammalsCompare: ContrVOrdering[Mammal] = new ContrVOrdering[Mammal] {
    override def compare(a: Mammal, b: Mammal): Int = {
      a.name compare(b.name)
    }
  }
  implicit val animalCompare: ContrVOrdering[Animal] = new ContrVOrdering[Animal] {
    override def compare(a: Animal, b: Animal): Int = {
      a.name compare(b.name)
    }
  }

  implicit val intCompare: ContrVOrdering[Int] = new ContrVOrdering[Int] {
    override def compare(a: Int, b: Int): Int = {
      a.compare(b)
    }
  }
}

