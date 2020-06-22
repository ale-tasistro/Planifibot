class Evento(n:String) {

  val nombre:String = n
  private var listaHorarios:List[Horario] = Nil

  // llama a buscarDia, y pide horas al horario resultante
  def agregarHorario(): Unit = {
    println("Para qué día querés agregar horas?")
    val dia: String = io.StdIn.readLine(); val h: Horario = buscarDia(dia, listaHorarios)
    h.pedirHoras()
    println("Querés agregar horas para otro día?")
    io.StdIn.readLine() match {
      case "si" => agregarHorario()
      case "no" => println("A las órdenes.")
      case _ => println("Me voy porque no entendí.")
    }
  }
  // busca un horario con día "dia" en "lista", si lo encuentre lo devuelve,
  // si no, crea uno nuevo, lo agrega al principio de listaHorarios, y lo devuelve
  @scala.annotation.tailrec
  private def buscarDia(dia: String, lista: List[Horario]) : Horario =
    lista match {
      case Nil => val h: Horario = new Horario(dia)
        listaHorarios =  h::listaHorarios
        h
      case x::xs => if (x.dia == dia) x else buscarDia(dia, xs)
    }

  // devuelve una lista con los marzullos de cada elemento de listaHorarios
  def coordinar(): List[(String, Int, (String, String))] = {
    listaHorarios.map(x => x.coord())
  }

}
