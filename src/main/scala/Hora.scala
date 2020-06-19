case class Hora(hora : Int, minuto : Int, flag : Int) {
  require(hora >= 0 && 0 <= minuto && minuto < 60, "Ingrese una hora válida, por favor.")

  def horaString: String = {
    s"$hora : $minuto"
  }

  def respecto(y:Hora): String = {
    if (horaString == y.horaString) {
      "igual"
    } else {
      if (horaString < y.horaString) {
        "menor"
      } else {
        "mayor"
      }
    }
  }

}
