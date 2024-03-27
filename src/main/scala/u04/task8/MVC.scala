package u04.task8

@main def runMVC =
  import DrawMyNumberStateImpl.*
  import Monads.*
  import Monad.*
  import States.*
  import State.*
  import WindowStateImpl.*
  import u03.extensionmethods.Streams.*
  
  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] = 
    State: (sm, sv) => 
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for 
    _ <- setSize(300, 300)
    _ <- addLabel(text = str, name = "Number")
    _ <- addButton(text = "draw", name = "DrawButton")
    _ <- addTextField(text = "", name = "NumberString", size = 10)
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(get(), i => windowCreation(i.toString()))
    _ <- seqN(events.map(_ match
        case "DrawButton" => mv(seq(set(fromTextField("NumberString")), get()), i => toLabel(i.toString, "Number"))
    ))
  yield ()

  controller.run((initialNumber(), initialWindow))