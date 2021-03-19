import scala.collection.mutable.ListBuffer
import scala.util.Random

class Paciente(val nombre:String, val primerAp:String, val segundoAp:String, val edad:Byte) {
  private val cantidadDatos = 20
  val fechaRegistro = new ListBuffer[String]()
  llenarListaFechas()
  val horaRegistro = new ListBuffer[String]()
  llenarListaHoras()
  val nivelBienestar = new ListBuffer[String]()
  llenarListaBienestar()
  val temperatura = new ListBuffer[Double]()
  llenarListaTemperatura()
  val humedad = new ListBuffer[Double]()
  llenarListaHumedad()


  //Metodos para llenar las listas con valores aleatorios.
  private def llenarListaFechas() : Unit = {
    for (_ <- 1 to cantidadDatos) fechaRegistro += Random.between(1, 30).toString + "/" + Random.between(1, 13).toString + "/2020"
  }

  private def llenarListaHoras() : Unit = {
    for (_ <- 1 to cantidadDatos) horaRegistro += Random.between(0, 23).toString + ":" + Random.between(0, 59).toString
  }

  private def llenarListaBienestar() : Unit = {
    for (_ <- 1 to cantidadDatos) nivelBienestar += "Nivel " + Random.between(1, 6).toString
  }

  private def llenarListaTemperatura() : Unit = {
    for (_ <- 1 to cantidadDatos) temperatura += 10 + ( 40 - 10 ) * Random.nextDouble()
  }

  private def llenarListaHumedad() : Unit = {
    for (_ <- 1 to cantidadDatos) humedad += 10 + ( 60 - 10 ) * Random.nextDouble()
  }

  //Metodos para mostrar el contenido de las listas.
  private def mostrarListasStrings(lista:ListBuffer[String]) : Unit = {
    print("[ ")
    for (e <- lista.indices) if(e == lista.length-1) print(lista(e)) else print(lista(e) + ", ")
    println(" ]")
  }

  private def mostrarListasDouble(lista:ListBuffer[Double]) : Unit = {
    print("[ ")
    for (e <- lista.indices) if(e == lista.length-1) print(lista(e)) else print(lista(e) + ", ")
    println(" ]")
  }

  //Metodos para mostrar el contenido de todas las listas.
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


  def mostrarListaPromedios(lista:ListBuffer[Double]) : Unit = {
    for (e <- lista.indices) println("\nPromedio del nivel " + (e+1) + ": " + lista(e))
  }

  def obtenerPromediosBienestar() : ListBuffer[Double] = {
    val promedios = ListBuffer[Double]()
    var cont1 = 0
    var cont2 = 0
    var cont3 = 0
    var cont4 = 0
    var cont5 = 0

    for (i <- nivelBienestar.indices) {
      if(nivelBienestar(i).equals("Nivel 1")) cont1 += 1
      else if(nivelBienestar(i).equals("Nivel 2")) cont2 += 1
      else if(nivelBienestar(i).equals("Nivel 3")) cont3 += 1
      else if(nivelBienestar(i).equals("Nivel 4")) cont4 += 1
      else if(nivelBienestar(i).equals("Nivel 5")) cont5 += 1
    }

    promedios += cont1 / nivelBienestar.length.toDouble
    promedios += cont2 / nivelBienestar.length.toDouble
    promedios += cont3 / nivelBienestar.length.toDouble
    promedios += cont4 / nivelBienestar.length.toDouble
    promedios += cont5 / nivelBienestar.length.toDouble

    promedios
  }
}


object Prueba {

  def main(args: Array[String]): Unit = {
    val paciente1 = new Paciente("Cristofer", "Casas", "Murillo", 21.toByte)
    paciente1.mostrarListas()
    paciente1.mostrarListaPromedios(paciente1.obtenerPromediosBienestar())

  }
}