class Evento(n:String) {
  type HorasQuien = (Horario, String)
  def Qhorarios(hQ:HorasQuien) : Horario = {
    hQ._1
  }
  def hQuien(hQ:HorasQuien) : String = {
    hQ._2
  }

  val nombre:String = n
  private var listaHorarios:List[HorasQuien] = _

  def agregarHorario(quien: String, horario: Horario): Unit = {
    val dupla: HorasQuien = (horario, quien)
    agregarAgrupado(dupla, listaHorarios)
  } // Esta función es pa llamarla tras haber terminado de guardar las horas disponibles

  def agregarAgrupado(dupla: HorasQuien, lista: List[HorasQuien]): Unit = {
    lista match {
      case Nil => listaHorarios = dupla::listaHorarios
      case x::xs =>
        if (Qhorarios(dupla).dia == Qhorarios(x).dia)
          Qhorarios(dupla).mergeHorarios(Qhorarios(x))
        else agregarAgrupado(dupla,xs)
    }
  }

}
