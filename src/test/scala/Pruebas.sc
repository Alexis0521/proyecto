import Oraculo.*
import PruebaArbolSufijos.reconstruirCadenaTurboPar
import ReconstCadenas.*
import ReconstCadenasPar.*

import scala.annotation.tailrec
import scala.util.Random
import _root_.Oraculo.alfabeto
import ReconstCadenasPar.Main.{costoOraculo, secsLargasParaPruebas}

val random = new Random()

@tailrec
def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long, alfabeto(indiceAzar)+:s)
  }
}
// PRUEBAS PARA RECONSTRUIR CADENA TURBO Y TURBO PARALELA



// Secuencias para pruebas turbo (potencias de 2)
val secTurbo1 = Seq('a', 'c', 'c', 'a') // longitud 4
val secTurbo2 = Seq('a', 'g', 'g', 'a', 't', 'c', 'c', 'a') // longitud 8
val secTurbo3 = secAlAzar(8, Seq())
val secTurbo4 = secAlAzar(16, Seq())

val orTurbo1 = crearOraculo(costoOraculo)(secTurbo1)
val orTurbo2 = crearOraculo(costoOraculo)(secTurbo2)
val orTurbo3 = crearOraculo(costoOraculo)(secTurbo3)
val orTurbo4 = crearOraculo(costoOraculo)(secTurbo4)

// Prueba secuencial
println(reconstruirCadenaTurbo(secTurbo1.length, orTurbo1))
println(reconstruirCadenaTurbo(secTurbo2.length, orTurbo2))
println(reconstruirCadenaTurbo(secTurbo3.length, orTurbo3))
println(reconstruirCadenaTurbo(secTurbo4.length, orTurbo4))

// Prueba paralela (umbral = 5, puedes jugar con este valor)
println(reconstruirCadenaTurboPar(5)(secTurbo1.length, orTurbo1))
println(reconstruirCadenaTurboPar(5)(secTurbo2.length, orTurbo2))
println(reconstruirCadenaTurboPar(5)(secTurbo3.length, orTurbo3))
println(reconstruirCadenaTurboPar(5)(secTurbo4.length, orTurbo4))

// Pruebas con varias secuencias generadas al azar
val turboSeqs = secsLargasParaPruebas(5) // secuencias de 2, 4, 8, 16, 32, 64, ...

def pruebasTurbo(ss: Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaTurbo(s.length, o))

def pruebasTurboPar(ss: Seq[Seq[Char]], umbral: Int) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaTurboPar(umbral)(s.length, o))

// Ejecutar pruebas
pruebasTurbo(turboSeqs)
pruebasTurboPar(turboSeqs, 5)

// Pruebas de casos límite y especiales
val especiales = Seq(
  Seq('a'),
  Seq('a', 'a', 'a', 'a'),
  Seq('a', 'c', 'a', 'c', 'a', 'c', 'a', 'c'),
  Seq('c', 'g', 't', 'a', 'c', 'g', 't', 'a'),
  Seq.fill(16)('g')
)
especiales.foreach { s =>
  val o = crearOraculo(costoOraculo)(s)
  val turbo = reconstruirCadenaTurbo(s.length, o)
  val par = reconstruirCadenaTurboPar(5)(s.length, o)
  println(s"Original: $s\nTurbo: $turbo\nPar: $par\nTest: ${turbo == s && par == s}\n")
}

// Pruebas de rendimiento 
def testTiempo(f: => Unit): Long = {
  val t0 = System.nanoTime()
  f
  (System.nanoTime() - t0)/1000000
}
val grandes = Seq(32, 64, 128).map(n => secAlAzar(n, Seq()))
grandes.foreach { s =>
  val o = crearOraculo(costoOraculo)(s)
  val t = testTiempo { reconstruirCadenaTurbo(s.length, o) }
  val tPar = testTiempo { reconstruirCadenaTurboPar(5)(s.length, o) }
  println(s"n=${s.length}: Turbo=${t}ms, TurboPar=${tPar}ms")
}