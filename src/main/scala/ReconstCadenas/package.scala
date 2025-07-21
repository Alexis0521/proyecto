import ArbolSufijos.*
import Oraculo.*

import scala.annotation.tailrec

package object ReconstCadenas {
  

  def reconstruirCadenaMejorado(n: Int, o: Oraculo.Oraculo): Seq[Char] = {
    val alfabeto = Oraculo.alfabeto

    // Función auxiliar para expandir SC(k-1) con todas las letras del alfabeto
    def expandir(sc: Set[Seq[Char]]): Set[Seq[Char]] = {
      sc.flatMap { w => alfabeto.map(letra => w :+ letra) }
    }

    // Función recursiva que construye SCk paso a paso
    def construir(k: Int, scAnterior: Set[Seq[Char]]): Seq[Char] = {
      val scActual = expandir(scAnterior).filter(o)
      scActual.find(_.length == n) match {
        case Some(cad) => cad
        case None if k < n => construir(k + 1, scActual)
        case _ => throw new RuntimeException("No se encontró cadena válida.")
      }
    }

    // Comenzamos con SC0 = {""}
    construir(1, Set(Seq()))
  }

  def imprimirTrie(t: Trie, nivel: Int = 0): Unit = {
    val indent = "  " * nivel
    t match {
      case Nodo(c, marcada, hijos) =>
        println(s"$indent- Nodo('$c') marcada: $marcada")
        hijos.foreach(h => imprimirTrie(h, nivel + 1))
      case Hoja(c, marcada) =>
        println(s"$indent- Hoja('$c') marcada: $marcada")
    }
  }


    def extraerPalabras(trie: Trie): Seq[Seq[Char]] = {

      def recorrer(actual: Trie, prefijo: Seq[Char]): Seq[Seq[Char]] = actual match {
        case Hoja(c, marcada) =>
          if (marcada) Seq(prefijo :+ c)
          else Seq.empty

        case Nodo(c, marcada, hijos) =>
          val nuevoPrefijo = if (c == '_') prefijo else prefijo :+ c

          val palabraActual =
            if (marcada && c != '_') Seq(nuevoPrefijo) else Seq.empty
          //Explora todos los nodos del Trie
          val palabrasDesdeHijos = hijos.flatMap(hijo =>
            recorrer(hijo, nuevoPrefijo)
          )

          palabraActual ++ palabrasDesdeHijos
      }

      recorrer(trie, Seq.empty)
    }



}

