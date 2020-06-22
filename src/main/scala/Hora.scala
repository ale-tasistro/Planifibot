case class Hora(hora : Int, minuto : Int) {
  require(hora >= 0 && 0 <= minuto && minuto < 60, "Una hora válida, por favor.")

  def horaString: String =
    if (minuto==0) s"$hora : 00"
    else s"$hora : $minuto"

  def respecto(y:Hora): String =
    if (horaString < y.horaString) "menor"
      else if (horaString == y.horaString) "igual"
        else "mayor"

}
