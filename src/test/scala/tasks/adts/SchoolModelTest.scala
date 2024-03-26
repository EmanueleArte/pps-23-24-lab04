package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import SchoolModel.SchoolModuleImpl.*
import org.junit.jupiter.api.Assertions.assertAll
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*

/* Tests should be clear, but note they are expressed independently of the
   specific implementation
*/

class SchoolModelTest:

  val courses = Cons(CourseImpl("Math"), Cons(CourseImpl("Physics"), Nil()))

  @Test def testAddTeacher(): Unit =
    val baseSchool = SchoolImpl(Nil(), Nil())
    val teacher1 = TeacherImpl("John", Nil())
    val teacher2 = TeacherImpl("Hannah", Nil())
    val school1 = baseSchool.addTeacher("John")
    assertAll(
      () => assertEquals(Cons(teacher1, Nil()), school1.teachers),
      () => assertEquals(Cons(teacher2, Cons(teacher1, Nil())), school1.addTeacher("Hannah").teachers)
    )

  @Test def testAddCourse(): Unit =
    val baseSchool = SchoolImpl(Nil(), Nil())
    val course1 = CourseImpl("Math")
    val course2 = CourseImpl("Physics")
    val school1 = baseSchool.addCourse("Math")
    assertAll(
      () => assertEquals(Cons(course1, Nil()), school1.courses),
      () => assertEquals(Cons(course2, Cons(course1, Nil())), school1.addCourse("Physics").courses)
    )

  @Test def testTeacherByName(): Unit =
    val teacher1 = TeacherImpl("John", Nil())
    val teacher2 = TeacherImpl("Hannah", Nil())
//    val school = SchoolImpl(Cons(teacher2, Cons(teacher1, Nil())), Nil())
    val school = SchoolImpl(Nil(), Nil()).addTeacher("John").addTeacher("Hannah")
    assertAll(
      () => assertEquals(Just(teacher1), school.teacherByName("John")),
      () => assertEquals(Just(teacher2), school.teacherByName("Hannah")),
      () => assertEquals(Empty(), school.teacherByName("Peter"))
    )

  @Test def testCourseByName(): Unit =
    val course1 = CourseImpl("Math")
    val course2 = CourseImpl("Physics")
    val school = SchoolImpl(Nil(), Nil()).addCourse("Math").addCourse("Physics")
    assertAll(
      () => assertEquals(Just(course1), school.courseByName("Math")),
      () => assertEquals(Just(course2), school.courseByName("Physics")),
      () => assertEquals(Empty(), school.courseByName("Chemistry"))
    )

  @Test def testCoursesOfTeacher(): Unit =
    val teacher1 = TeacherImpl("John", Nil())
    val teacher2 = TeacherImpl("Hannah", Nil())
    val baseSchool = SchoolImpl(Nil(), Nil())
    val school1 = baseSchool.addTeacher("John").addTeacher("Hannah")
    val school2 = school1.addCourse("Math").addCourse("Physics")
    val school3 = school2.setTeacherToCourse(
      orElse(school2.teacherByName("John"), null), orElse(school2.courseByName("Math"), null)
    )
    assertAll(
      () => assertEquals(Cons(CourseImpl("Math"), Nil()), school3.coursesOfATeacher(orElse(school3.teacherByName("John"), null))),
      () => assertEquals(Nil(), school3.coursesOfATeacher(teacher1))
    )

//  @Test def testNameOfTeacher(): Unit =
//    val baseSchool = SchoolImpl(Nil(), Nil())
//    val teacher1 = TeacherImpl("John", courses)
//    val teacher2 = TeacherImpl("Hannah", Nil())
//    val school = baseSchool.addTeacher("John").addTeacher("Hannah")
//    assertAll(
//      () => assertEquals("John", school.nameOfTeacher(teacher1)),
//      () => assertEquals("Hannah", school.nameOfTeacher(teacher2))
//    )