import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import scala.annotation.tailrec
import scala.util.Random
import Benchmark.Comparacion._

object Main {
  val random = new Random()
  val costoOraculo = 1

  @tailrec
  def secAlAzar(long: Int, s: Seq[Char]): Seq[Char] = {
    if (s.length == long) s
    else {
      val indiceAzar = random.nextInt(4)
      secAlAzar(long, alfabeto(indiceAzar) +: s)
    }
  }

  def secsCortasParaPruebas(n: Int): Seq[Seq[Char]] = for {
    i <- 1 to n
    s = secAlAzar(i, Seq())
  } yield s

  def secsLargasParaPruebas(n: Int): Seq[Seq[Char]] = for {
    i <- 1 to n
    s = secAlAzar(math.pow(2, i).toInt, Seq())
  } yield s

  def pruebasTurboAcelerada(ss: Seq[Seq[Char]]): Unit = {
    println(s"Ejecutando pruebas con ${ss.length} secuencias...")
    ss.foreach { s =>
      val o = crearOraculo(costoOraculo)(s)
      val res = reconstruirCadenaTurboAcelerada(s.length, o)
      if (res == s) println(s"✔️ OK: ${s.length} caracteres")
      else println(s"ERROR: Esperado ${s.mkString}, Obtenido ${res.mkString}")
    }
  }

  def main(args: Array[String]): Unit = {
    // Secuencias para pruebas
    val ss1_10 = secsCortasParaPruebas(10)
    val ss1_16 = secsCortasParaPruebas(16)
    val ss2_1024 = secsLargasParaPruebas(10)
    val ss2_2048 = secsLargasParaPruebas(11)
    val ss2_4096 = secsLargasParaPruebas(12)

    val sets = Seq(
      ("ss1_10", ss1_10),
      ("ss1_16.slice(0,10)", ss1_16.slice(0,10)),
      ("ss1_16.slice(0,11)", ss1_16.slice(0,11)),
      ("ss1_16.slice(0,12)", ss1_16.slice(0,12)),
      ("ss1_16.slice(0,13)", ss1_16.slice(0,13)),
      ("ss1_16.slice(0,14)", ss1_16.slice(0,14)),
      ("ss1_16.slice(0,15)", ss1_16.slice(0,15)),
      ("ss1_16", ss1_16),
      ("ss2_1024", ss2_1024)
    )


    val a1 = reconstruirCadenaTurboAcelerada
    val a2 = reconstruirCadenaTurboAceleradaPar

    sets.foreach { (label, set) =>
      println(s"\n=== BENCHMARK PARA: $label ===")
      benchmarkSet(label, set, a1, a2)
    }
  }
}
