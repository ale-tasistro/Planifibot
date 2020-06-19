class Horario(d:String) {

  val dia:String = d
  private var horas:List[Hora] = _

  def agregarHora(hora:Hora) : Unit = {
    horas = agregarHoraAcc(hora, horas, Nil)
  } // inserta ordenado
  @scala.annotation.tailrec
  private def agregarHoraAcc(hora: Hora, list: List[Hora], aux: List[Hora]) : List[Hora] = {
    list match {
      case Nil => (hora::aux).reverse
      case x::xs => hora.respecto(x) match {
        case "menor" => (hora::list++aux).reverse
        case "igual" => (hora::list++aux).reverse    // habría que ver cómo mergear horas iguales
        case "mayor" => agregarHoraAcc(hora, xs, x::aux)
      }
    }
  }

  def mergeHorarios(horario:Horario): Unit = {
    mergeLHAcc(horas,horario.horas, Nil)
  }
  @scala.annotation.tailrec
  private def mergeLHAcc(l1: List[Hora], l2: List[Hora], aux: List[Hora]): List[Hora] = l1 match {
    case Nil => aux.reverse++l2  // se asume que l1, y l2 están ordenadas
    case x::xs =>
      l2 match {
        case Nil => aux.reverse++l1
        case y::ys => x.respecto(y) match {
          case "menor" => mergeLHAcc(xs, l2, x::aux)
          case "igual" => mergeLHAcc(xs, l2, x::aux)
          case "mayor" => mergeLHAcc(l1, ys, y::aux)
        }

      }
  }

  def coord(): (Int, (String, String)) = {
    marzullo(l = horas, aux = 0, best = 0, cuando =("",""))
  }
  @scala.annotation.tailrec
  private def marzullo(l: List[Hora], aux: Int, best: Int, cuando: (String, String)):  (Int, (String, String)) = {
    // l se asume ordenada
    l match {
      case Nil => (best, cuando)
      case x::xs =>
        val nuevoAux = aux + x.flag
        if (nuevoAux > best) {
          val nuevoBest = nuevoAux
          val nuevoCuando = (x.horaString, xs.head.horaString)
          marzullo(xs, nuevoAux, nuevoBest, nuevoCuando)
        } else {
          marzullo(xs, nuevoAux, best, cuando)
        }
    }
  }
}

