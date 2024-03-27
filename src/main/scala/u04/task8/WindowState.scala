package u04.task8

import u03.extensionmethods.Streams.*
import u04.task8.Monads.*
import u04.task8.Monads.Monad.*
import u04.task8.States.*
import u04.task8.States.State.*

trait WindowState:
  type Window
  def initialWindow: Window
  def setSize(width: Int, height: Int): State[Window, Unit]
  def addButton(text: String, name: String): State[Window, Unit]
  def addLabel(text: String, name: String): State[Window, Unit]
  def addTextField(text: String, name: String, size: Int): State[Window, Unit]
  def toLabel(text: String, name: String): State[Window, Unit]
  def fromTextField(name: String): String
  def show(): State[Window, Unit]
  def exec(cmd: =>Unit): State[Window, Unit]
  def eventStream(): State[Window, Stream[String]]

object WindowStateImpl extends WindowState:
  import SwingFunctionalFacade.*
  
  type Window = Frame
  
  def initialWindow: Window = createFrame

  def setSize(width: Int, height: Int): State[Window, Unit] = 
    State(w => ((w.setSize(width, height)), {}))
  def addButton(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addButton(text, name)), {}))
  def addLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addLabel(text, name)), {}))
  def addTextField(text: String, name: String, size: Int): State[Window, Unit] =
    State(w => ((w.addTextField(text, name, size)), {}))
  def toLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.showToLabel(text, name)), {}))
  def fromTextField(name: String): String =
      getFrame.getTextFieldValue(name)
  def show(): State[Window, Unit] =
    State(w => (w.show, {}))
  def exec(cmd: =>Unit): State[Window, Unit] =
    State(w => (w, cmd))  
  def eventStream(): State[Window, Stream[String]] =
    State(w => (w, Stream.generate(() => w.events().get)))
