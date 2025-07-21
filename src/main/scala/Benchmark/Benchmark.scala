package Benchmark

import Oraculo.*
import ReconstCadenas.*
import org.scalameter._
import scala.util.Random

object Comparacion {

  type AlgoritmoReconstruccion = (Int, Oraculo) => Seq[Char]

  def compararAlgoritmos(
                          a1: AlgoritmoReconstruccion,
                          a2: AlgoritmoReconstruccion,
                          s: Seq[Char]
                        ): (Double, Double, Double) = {
    val o1 = crearOraculo(1)(s)
    val o2 = crearOraculo(1)(s)

    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns, 20),
      KeyValue(Key.exec.maxWarmupRuns, 60),
      KeyValue(Key.verbose, false)
    ) withWarmer(new Warmer.Default) measure {
      val res = a1(s.length, o1)
      assert(res == s)
    }

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns, 20),
      KeyValue(Key.exec.maxWarmupRuns, 60),
      KeyValue(Key.verbose, false)
    ) withWarmer(new Warmer.Default) measure {
      val res = a2(s.length, o2)
      assert(res == s)
    }

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  def benchmarkSet(
                    nombreSet: String,
                    ss: Seq[Seq[Char]],
                    a1: AlgoritmoReconstruccion,
                    a2: AlgoritmoReconstruccion
                  ): Unit = {
    println(s"\n>>> Comparando algoritmos en set: $nombreSet <<<")

    ss.foreach { s =>
      val (t1, t2, speedup) = compararAlgoritmos(a1, a2, s)
      println(f"[n=${s.length}%4d] A1: $t1%.4f s | A2: $t2%.4f s | Speedup: $speedup%.2f×")
    }
  }
}

