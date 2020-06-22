class Horario(d:String) {

  type HoraFlag = (Hora, Int)
  def hora(h: HoraFlag): Hora = h._1
  def flag(h: HoraFlag): Int = h._2

  val dia: String = d
  private var horas: List[HoraFlag] = Nil

  // lee el input y arma una intervalo con dos horas, la primera debe ser menor que la segunda
  @scala.annotation.tailrec
  private def entradaSalida(): List[HoraFlag] = {

    println("A partir de qué hora podés? (hora minuto)")
    val hE : String = io.StdIn.readLine()
    val splitE: Array[String] = hE.split(' ')
    val entrada: Hora = Hora(splitE(0).toInt, splitE(1).toInt)

    println("Y hasta qué hora?")
    val hS : String = io.StdIn.readLine()
    val splitS: Array[String] = hS.split(" ")
    val salida: Hora = Hora(splitS(0).toInt, splitS(1).toInt)

    entrada.respecto(salida) match {
      case "menor" => (entrada, 1) :: (salida, -1) :: Nil
      case "igual" => println("Error: las horas son iguales")
        entradaSalida()
      case "mayor" => println("Error: la hora final es anterior a la inicial")
        entradaSalida()
    }
  }

  // pide el primer intervalo, y llama a pedirHorasAcc armar una lista con todos los que se quiera
  // finalmente llama a mergeListHAcc para agregar los nuevos intervalos a la lista de intervalos preexistente
  def pedirHoras(): Unit = {
    val lista: List[(Hora, Int)] = entradaSalida()
    println("Querés agregar más horas?")
    val aux: List[(Hora, Int)] = pedirHorasAcc(lista)
    horas = mergeListHAcc(horas, aux, Nil)
  }
  @scala.annotation.tailrec
  private def pedirHorasAcc(res: List[HoraFlag]): List[(Hora, Int)] = {
    //esta función es llamada exclusivamente por pedirHoras() y por ella misma,
    // comienza esperando respuesta a si se quiere agregar más horas
    io.StdIn.readLine().toLowerCase match {
      case "no" => res  // en res se pasa la lista con las horas entradas hasta el momento
      case "si" => val lista: List[(Hora, Int)] = entradaSalida() // para agregar horas nuevas se crea un intervalo
        println("Querés agregar más horas?")
        pedirHorasAcc(mergeListHAcc(lista, res, Nil)) // y se lo mergea con res
      case _ => println("Si o no, por favor.")
        pedirHorasAcc(res)
    }
  }
  @scala.annotation.tailrec
  private def mergeListHAcc(l1: List[HoraFlag], l2: List[HoraFlag], aux: List[HoraFlag]): List[HoraFlag] =
    l1 match {
      case Nil => aux.reverse ++ l2 // se asume que l1, y l2 están ordenadas
      case x::xs =>
        l2 match {
          case Nil => aux.reverse ++ l1
          case y::ys => hora(x).respecto(hora(y)) match { // si ninguna de las listas a mergear está vacía se comparan sus cabezas
            case "menor" => mergeListHAcc(xs, l2, x :: aux) // la que tenga la hora más chica se va de su lista y pasa a ser la cabeza de aux
            case "mayor" => mergeListHAcc(l1, ys, y :: aux)
            case "igual" => val nuevaFlag: Int = flag(x) + flag(y) // si tienen la misma hora se suman las flags
              if ( nuevaFlag == 0) { // si la suma es 0 se descartan ambas cabezas y se pasa sin cambiar aux (llego alguien y se fue alguien)
              mergeListHAcc(xs, ys, aux)
              } else {// si no, se pasa hora(x)(== hora(y)) con la suma de las flags como nueva cabeza de aux, y se descartan las dos cabezas
                mergeListHAcc(xs, ys, (hora(x), nuevaFlag) :: aux)
              }
          }
        }
    }

  def coord(): (String, Int, (String, String)) = { // llama a marzullo con los valores iniciales adecuados
    val (cant: Int, cuando: (String, String)) = marzullo(l = horas, aux = 0, best = 0, cuando = ("", ""))
    (dia, cant, cuando)
  }
  @scala.annotation.tailrec
  private def marzullo(l: List[(Hora, Int)], aux: Int, best: Int, cuando: (String, String)): (Int, (String, String)) = {
    // l se asume ordenada,
    // en aux se pasa la suma de los flags hasta el momento,
    // en best el máximo valor que haya tenido aux,
    // y en cuando se guardan, como strings, las horas entre las que aux vale best
    l match {
      case Nil => (best, cuando) // al terminar best es el máximo valor de aux y cuando son las horas en que aux vale best
      case x :: xs =>
        val nuevoAux: Int = aux + flag(x) //flag(x) es 1 o -1, indica si a esta hora alguien se suma o alguien se va
        if (nuevoAux > best) {
          val nuevoBest: Int = nuevoAux
          val nuevoCuando: (String, String) = (hora(x).horaString, hora(xs.head).horaString) // xs.head existe porque nuevoAux > best
          // solo puede pasar si flag==1, por lo que tiene que haber una hora posterior con flag== -1
          marzullo(xs, nuevoAux, nuevoBest, nuevoCuando)
        } else {
          marzullo(xs, nuevoAux, best, cuando)
        }
    }
  }

}

