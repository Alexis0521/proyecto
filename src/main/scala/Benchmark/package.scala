import Oraculo._
import scala.collection.parallel.CollectionConverters._
import org.scalameter._
package object Benchmark {

  type AlgoritmoReconstruccion = (Int, Oraculo) => Seq[Char]
  def compararAlgoritmos(a1: AlgoritmoReconstruccion, a2: AlgoritmoReconstruccion)
                        (n: Int, s: Seq[Char]): (Double, Double, Double) = {
    val o = crearOraculo(1)(s)

    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns, 20),
      KeyValue(Key.exec.maxWarmupRuns, 60),
      KeyValue(Key.verbose, false)
    ) withWarmer (new Warmer.Default) measure {
      val r = a1(n, o)
      assert(r == s)
    }
    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns, 20),
      KeyValue(Key.exec.maxWarmupRuns, 60),
      KeyValue(Key.verbose, false)
    ) withWarmer (new Warmer.Default) measure {
      val r = a2(n, o)
      assert(r == s)
    }

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

}
