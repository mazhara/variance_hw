package variance.hw

sealed abstract class Animal(val name: String)

class Mammal(name: String) extends Animal(name)
class Pet(name: String) extends Mammal(name)
class Cat(name: String) extends Pet(name)
class Dog(name: String) extends Pet(name)
