import scala.collection.mutable.ListBuffer
import scala.util.Random

class Paciente(val nombre:String, val primerAp:String, val segundoAp:String, val edad:Byte) {
  val fechaRegistro = new ListBuffer[String]()
  val horaRegistro = new ListBuffer[String]()
  val nivelBienestar = new ListBuffer[String]()
  val temperatura = new ListBuffer[Double]()
  val humedad = new ListBuffer[Double]()
  llenarListas()


  private def llenarListaFechas() : Unit = {
    for (_ <- 1 to 15) fechaRegistro += Random.between(1, 30).toString + "/" + Random.between(1, 13).toString + "/2020"
  }

  private def llenarListaHoras() : Unit = {
    for (_ <- 1 to 15) horaRegistro += Random.between(0, 23).toString + ":" + Random.between(0, 59).toString
  }

  private def llenarListaBienestar() : Unit = {
    for (_ <- 1 to 15) nivelBienestar += "Nivel " + Random.between(1, 6).toString
  }

  private def llenarListaTemperatura() : Unit = {
    for (_ <- 1 to 15) temperatura += 10 + ( 40 - 10 ) * Random.nextDouble()
  }

  private def llenarListaHumedad() : Unit = {
    for (_ <- 1 to 15) humedad += 10 + ( 60 - 10 ) * Random.nextDouble()
  }

  def mostrarListasStrings(lista:ListBuffer[String]) : Unit = {
    print("[ ")
    for (e <- lista.indices) if(e == lista.length-1) print(lista(e)) else print(lista(e) + ", ")
    println(" ]")
  }

  def mostrarListasDouble(lista:ListBuffer[Double]) : Unit = {
    print("[ ")
    for (e <- lista.indices) if(e == lista.length-1) print(lista(e)) else print(lista(e) + ", ")
    println(" ]")
  }

  private def llenarListas() : Unit = {
    llenarListaFechas()
    llenarListaHoras()
    llenarListaBienestar()
    llenarListaTemperatura()
    llenarListaHumedad()
  }

  def mostrarListas() : Unit = {
    mostrarListasStrings(fechaRegistro)
    println()
    mostrarListasStrings(horaRegistro)
    println()
    mostrarListasStrings(nivelBienestar)
    println()
    mostrarListasDouble(temperatura)
    println()
    mostrarListasDouble(humedad)
  }

}


object Prueba {

  def main(args: Array[String]): Unit = {
    val paciente1 = new Paciente("Cristofer", "Casas", "Murillo", 21.toByte)
    paciente1.mostrarListas()

  }
}