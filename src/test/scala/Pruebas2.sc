import Oraculo.*
import ReconstCadenas.*

import scala.annotation.tailrec
import scala.util.Random

val random = new Random()

@tailrec
def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  //Crea una secuencia de long caracteres del alfabeto,
  // escogidos de forma aleatoria, terminando en s
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}
val costoOraculo = 1

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
val sec3=secAlAzar(10,Seq())

val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)

def pruebasTurboAcelerada(ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaTurboAcelerada(s.length,o))


reconstruirCadenaTurboAcelerada(sec1.length, crearOraculo(costoOraculo)(sec1))
reconstruirCadenaTurboAcelerada(sec2.length, crearOraculo(costoOraculo)(sec2))