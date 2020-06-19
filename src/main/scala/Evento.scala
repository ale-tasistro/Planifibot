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
  } // Esta función es pa llamarla tras haber terminado de armar el horario

  @scala.annotation.tailrec
  private def agregarAgrupado(dupla: HorasQuien, lista: List[HorasQuien]): Unit = {
    lista match {
      case Nil => listaHorarios = dupla::listaHorarios
      case x::xs =>
        if (Qhorarios(dupla).dia == Qhorarios(x).dia)
          Qhorarios(dupla).mergeHorarios(Qhorarios(x))
        else agregarAgrupado(dupla,xs)
    }
  }

  def coordinar(): List[(Int, (String, String))] = {
    listaHorarios.map(x => Qhorarios(x).coord())
  }



}
