package tasks.adts

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

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
  //      def teacherByName(name: String): Optional[Teacher]
  //      def courseByName(name: String): Optional[Course]
  //      def nameOfTeacher(teacher: Teacher): String
  //      def nameOfCourse(teacher: Teacher): String
  //      def setTeacherToCourse(teacher: Teacher, course: Course): School
  //      def coursesOfATeacher(teacher: Teacher): Sequence[Course]


  case class CourseImpl(name: String)

  case class TeacherImpl(name: String, courses: Sequence[CourseImpl])

  case class SchoolImpl(teachers: Sequence[TeacherImpl], courses: Sequence[CourseImpl])

  object SchoolModuleImpl extends SchoolModule:
    type School = SchoolImpl
    type Teacher = TeacherImpl
    type Course = CourseImpl

    extension (school: School)
      def addTeacher(name: String): School = SchoolImpl(Cons(TeacherImpl(name, Nil()), school.teachers), school.courses)

      def addCourse(name: String): School = SchoolImpl(school.teachers, Cons(CourseImpl(name), school.courses))