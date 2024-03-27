package alltasks

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*
import u04.monads.Monads.Monad

// Task 1 - Svolto da solo
object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex

    def complex(re: Double, im: Double): Complex

    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  case class ComplexNumber(re: Double, im: Double)

  object BasicComplexADT extends ComplexADT:

    type Complex = ComplexNumber

    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)

    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = ComplexNumber(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = ComplexNumber(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case ComplexNumber(re, im) if re == 0 && im == 0 => "0.0"
        case ComplexNumber(re, im) if re == 0 => s"${im}i"
        case ComplexNumber(re, im) if im == 0 => s"$re"
        case ComplexNumber(re, im) => s"$re ${if im < 0 then "- " + (-im) else "+ " + im}i"


// Task 2 - Svolto da solo
object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(course: Course): String
      def nameOfCourse(teacher: Teacher): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]


  object SchoolModuleImpl extends SchoolModule:
    type School = SchoolImpl
    type Teacher = TeacherImpl
    type Course = CourseImpl

    extension (school: School)
      def addTeacher(name: String): School = SchoolImpl(Cons(TeacherImpl(name, Nil()), school.teachers), school.courses)

      def addCourse(name: String): School = SchoolImpl(school.teachers, Cons(CourseImpl(name), school.courses))

      def teacherByName(name: String): Optional[Teacher] = filter(school.teachers)(t => t.name == name) match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def courseByName(name: String): Optional[Course] = filter(school.courses)(c => c.name == name) match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def nameOfTeacher(course: Course): String = filter(school.teachers)(t => filter(t.courses)(c => c.name == course.name) match
        case Cons(_, _) => true
        case _ => false) match
        case Cons(h, _) => h.name
        case _ => ""

      def nameOfCourse(teacher: Teacher): String = filter(school.teachers)(t => t == teacher) match
        case Cons(h, _) => h.courses match
          case Cons(h, _) => h.name
          case _ => ""
        case _ => ""

      def setTeacherToCourse(teacher: Teacher, course: Course): School = filter(school.courses)(c => c == course) match
        case Cons(_, _) => SchoolImpl(Sequence.map(school.teachers)(t => if t == teacher then TeacherImpl(t.name, Cons(course, t.courses)) else t), school.courses)
        case _ => school

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = filter(school.teachers)(t => t == teacher) match
        case Cons(h, _) => h.courses
        case _ => Nil()


  import SchoolModuleImpl.*

  case class CourseImpl(name: String)

  case class TeacherImpl(name: String, courses: Sequence[Course])

  case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])


// Task 3 - Svolto da solo
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = stack match
        case Cons(h, t) => Cons(a, Cons(h, t))
        case _ => Cons(a, Nil())
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just((h, t))
        case _ => Optional.Empty()
      def asSequence(): Sequence[A] = stack match
        case Cons(h, t) => Cons(h, t)
        case _ => Nil()


// Task 4 - Svolto da solo
object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match
      case Cons(h, t) => summable.sum(h, sumAll(t))
      case _ => summable.zero

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""


// Task 5 - Svolto da solo
object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: " + a)

  def logAll[T[_]: Traversable, A](seq: T[A])(log: A => Unit): Unit =
    summon[Traversable[T]].foreach(seq)(log)

  trait Traversable[T[_]]:
    def foreach[A](seq: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def foreach[A](seq: Sequence[A])(f: A => Unit): Unit = seq match
      case Cons(h, t) => f(h); foreach(t)(f)
      case _ => ()

  given Traversable[Optional] with
    def foreach[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Just(a) => f(a)
      case _ => ()


// Task 6 - Svolto da solo
object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] =
    try success(expression)
    catch case e: Throwable => failure(e)

  extension [A](m: Try[A])
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)
    extension [A](m: Try[A])

      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value) => f(value)
        case TryImpl.Failure(exception) => failure(exception)