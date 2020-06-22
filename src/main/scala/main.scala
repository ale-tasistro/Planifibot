object main extends App {

  private var listaEventos: List[Evento] = Nil

  @scala.annotation.tailrec
  def main(): Unit = {
    println("Lista de opciones:")
    println("'nuevo evento' para crear un evento nuevo")
    println("'agregar horas' para agregar horas a un evento")
    println("'coordinar evento' para coordinar un evento ya creado")
    println("'salir' para terminar el programa")
    val comando = io.StdIn.readLine().toLowerCase
    comando match {
      case "nuevo evento" =>
        println("Qué nombre le querés poner al evento")
        val nuevo = io.StdIn.readLine().toLowerCase()
        insertarEvento(nuevo,listaEventos)
        main()
      case "agregar horas" =>
        println("Para qué evento querés agregar horas?")
        val nombre = io.StdIn.readLine().toLowerCase()
        horasPara(nombre)
        main()
      case "coordinar evento" =>
        println("Qué evento querés coordinar?")
        val evento = io.StdIn.readLine().toLowerCase()
        planificar(evento)
        main()
      case "salir" =>
        println("A las órdenes")
      case _ =>
        println("No entendí")
        main()
    }
  }

  // inserta un evento en una lista de eventos
  private def insertarEvento(nombreEvento: String, l: List[Evento]): Unit = {
    val evento : Evento = new Evento(nombreEvento)
    fetchEvento(evento.nombre, l) match { // busca un evento con su mismo nombre en la lista
      case Right(_: Boolean) => listaEventos = evento :: listaEventos // si no hay ninguno se inserta al principio
      case Left(_: Evento) => println("Ya hay un evento con ese nombre.") // si ya hay uno no hace nada e imprime un mensaje
    }
  }

  // busca un evento, por nombre, en una lista de eventos
  @scala.annotation.tailrec
  private def fetchEvento(nombre: String, l: List[Evento]): Either[Evento,Boolean] =
    l match {
      case Nil => Right(false)  // si no hay un evento con ese nombre devuelve False
      case x::xs => if (x.nombre == nombre) {
        Left(x) // si hay un evento con ese nombre lo devuelve
      } else { fetchEvento(nombre, xs)}
    }

  // agrega horas disponibles para un evento
  private def horasPara(nombreEvento: String): Unit = {
    val nombre: String = nombreEvento.toLowerCase
    val flag: Either[Evento, Boolean] = fetchEvento(nombre, listaEventos)  // busca el evento en listaEventos
    flag match {
      case Left(evento) => evento.agregarHorario()  // si lo encuentra le pide agregarHorario()
      case Right(_) => println("No hay un evento con ese nombre, querés crearlo?")  //si no avisa y ofrece crearlo
        io.StdIn.readLine().toLowerCase match {
          case "si"=> val nuevo: Evento = new Evento(nombre)  // si se responde sí lo crea y llama agregarHorario()
            listaEventos = nuevo::listaEventos
            println("Creado.")
            nuevo.agregarHorario()
          case "no" => println("A las órdenes") // si se responde no termina
          case _ => println("Me voy porque no entendí.") //error en otro caso
        }
    }
  }

  // busca el evento, si lo encuentra le pide coordinar() e imprime la lista, si no tira error
  private def planificar(nombreEvento: String): Unit = {
    fetchEvento(nombreEvento, listaEventos) match {
      case Left(evento) => impLista(evento.coordinar())
      case Right(_) => println("No hay un evento con ese nombre.")
    }
  }

  // imprime una lista resultado de evento.coordinar()
  @scala.annotation.tailrec
  private def impLista(lista: List[(String, Int, (String, String))]): Unit = {
    lista match {
      case Nil => print(".")
      case x::xs => println("%s pueden %d personas de %s a %s".format(x._1, x._2, x._3._1, x._3._2))
        impLista(xs)
    }
  }

  main()
}
