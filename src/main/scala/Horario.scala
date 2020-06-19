class Horario(d:String) {

  val dia:String = d
  private var horas:List[Hora] = _
  def agregarHora(hora:Hora) : Unit = {
    horas = agregarHoraAcc(hora, horas, Nil)
  } // inserta ordenado

  def agregarHoraAcc(hora: Hora, list: List[Hora], aux: List[Hora]) : List[Hora] = {
    list match {
      case Nil => (hora::aux).reverse
      case x::xs => hora.respecto(x) match {
        case "menor" => (hora::list++aux).reverse
        case "igual" => (hora::list++aux).reverse    // habría que ver cómo mergear horarios pal mismo día
        case "mayor" => agregarHoraAcc(hora, xs, x::aux)
      }
    }
  }


  def mergeHorarios(horario:Horario): Unit = {
    mergeLHAcc(horas,horario.horas, Nil)
  }

  def mergeLHAcc(l1: List[Hora], l2: List[Hora], aux: List[Hora]): List[Hora] = l1 match {
    case Nil => aux.reverse++l2
    case x::xs =>
      l2 match {
        case Nil => aux.reverse++l1
        case y::ys => x.respecto(y) match {
          case "menor" => mergeLHAcc(xs, l2, y::aux)
          case "igual" => mergeLHAcc(xs, l2, y::aux)
          case "mayor" => mergeLHAcc(l1, ys, x::aux)
        }

      }
  }
}

