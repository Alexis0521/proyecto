import ArbolSufijos.*
import Oraculo.*

import scala.annotation.tailrec

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo.Oraculo): Seq[Char] = {
    def generarTodasLasCadenas(n: Int): LazyList[Seq[Char]] = {
      if (n == 0) LazyList(Seq())
      else for {
        prefijo <- generarTodasLasCadenas(n - 1)
        letra <- Oraculo.alfabeto.to(LazyList)
      } yield prefijo :+ letra
    }

    generarTodasLasCadenas(n).find(o).getOrElse(
      throw new RuntimeException("No se encontró una cadena que el oráculo acepte.")
    )
  }

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
  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    val arbolVacio = Nodo('_', false, List())

    def generarCombinaciones(prefijo: Seq[Char], arbol: Trie, resultado:Trie) : Trie = {
      // Se detiene si el oraculo determina que el prefijo no pertenece
      if (!o(prefijo)) return resultado
      // Si el prefijo no es vacio agrega el prefijo al Trie.
      val nuevoArbol = if (prefijo.nonEmpty) adicionar(prefijo, arbol) else arbol

      // Si se llega a una palabra completa (longitud n), se agrega al Trie de resultados
      val resultadoActualizado =
        if (prefijo.length == n) adicionar(prefijo, resultado)
        else resultado
      // Explora todos los posibles caracteres que siguen
      val siguientes = alfabeto.filter(char => o(prefijo :+ char))

      //Procesa cada caracter de forma recursiva
      siguientes.foldLeft(resultadoActualizado) { (resAcumulado, char) =>
        val nuevoPrefijo = prefijo :+ char
        generarCombinaciones(nuevoPrefijo, nuevoArbol, resAcumulado)
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

    val arbolResultado = generarCombinaciones(Seq.empty, arbolVacio, arbolVacio)
    val palabras = extraerPalabras(arbolResultado)
    /* Para pruebas solamente
    imprimirTrie(arbolResultado)

    
    println(palabras)
    println("Palabras extraídas:")
    palabras.foreach(p => println(p.mkString))
    
     */
    // Retorna la palabra encontrada
    palabras.headOption.getOrElse(Seq.empty)
  }
  /*
    def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
      val arbolVacio = Nodo('_', false, List())
  
      def generarCombinaciones(prefijo: Seq[Char], arbol: Trie) : Option[Seq[Char]] = {
  
        if (!o(prefijo)) return None
  
        val nuevoArbol = if (prefijo.nonEmpty) adicionar(prefijo, arbol) else arbol
  
  
        val siguientes = alfabeto.filter(char => o(prefijo :+ char))
  
        alfabeto
          .filter(char => o(prefijo :+ char))
          .flatMap(char => generarCombinaciones(prefijo :+ char, nuevoArbol))
          .headOption
      }
      generarCombinaciones(Seq.empty,arbolVacio).getOrElse(Seq.empty)
    }*/
}
