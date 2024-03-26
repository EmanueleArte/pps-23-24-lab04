package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import SchoolModel.SchoolModuleImpl.*
import org.junit.jupiter.api.Assertions.assertAll
import u03.Sequences.*
import u03.Sequences.Sequence.*

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

