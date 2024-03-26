package tasks.adts

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

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