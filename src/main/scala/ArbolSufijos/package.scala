import scala.annotation.tailrec

package object ArbolSufijos {

  abstract class Trie

  case class Nodo(car: Char, marcada: Boolean,
                  hijos: List[Trie]) extends Trie

  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, t) => t.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    (s, t) match {
      case (Seq(), Nodo(_, marcada, _)) => marcada
      case (Seq(), Hoja(_, marcada)) => marcada

      case (c +: cs, Nodo(_, _, hijos)) =>
        hijos.exists(hijo => raiz(hijo) == c && pertenece(cs, hijo))

      case (c +: cs, Hoja(car, marcada)) =>
        c == car && cs.isEmpty && marcada
    }
  }


  def adicionar(s: Seq[Char], t: Trie): Trie = {

    def construirDesde(s: Seq[Char]): Trie = s match {
      case Seq(c) => Hoja(c, true)
      case c +: cs => Nodo(c, false, List(construirDesde(cs)))
      case Nil => throw new IllegalArgumentException("No se puede construir desde una cadena vacía")
    }

    (s, t) match {
      case (Seq(), Nodo(c, _, hijos)) =>
        // println(s"Marcando nodo $c")
        Nodo(c, true, hijos)

      case (Seq(), Hoja(c, _)) =>
        //  println(s"Marcando hoja $c")
        Hoja(c, true)

      case (c +: cs, Nodo(car, marcada, hijos)) =>
        // println(s"En Nodo($car): añadiendo $c +: $cs")

        // ¿Existe ya un hijo con carácter c?
        val (coincide, otros) = hijos.partition(h => raiz(h) == c)

        val nuevoHijo = coincide match {
          case Nil =>
            // println(s"No existe hijo con raíz $c, construyendo desde cero")
            construirDesde(s)
          case h :: Nil =>
            //println(s"Encontrado hijo con raíz $c, añadiendo recursivamente")
            adicionar(cs, h)
          case _ =>
            throw new Exception("Error: múltiples hijos con el mismo carácter")
        }

        Nodo(car, marcada, nuevoHijo :: otros.filter(h => raiz(h) != raiz(nuevoHijo)))

      case (c +: cs, Hoja(car, marcada)) =>
        //println(s"Convertir hoja $car a nodo para insertar $c +: $cs")
        val nuevoNodo = Nodo(car, marcada, Nil)
        adicionar(c +: cs, nuevoNodo)
    }
  }

  // Construye un Trie a partir de múltiples secuencias, insertando todos sus sufijos
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    require(ss.nonEmpty, "La secuencia de secuencias no puede estar vacía")

    val raiz: Trie = Nodo('*', false, Nil) // Nota: ahora es de tipo Trie

    val resultado: Trie = ss.flatMap { s =>
      s.indices.map(i => s.drop(i)) // Genera todos los sufijos de cada secuencia
    }.foldLeft(raiz) { (trie, sufijo) =>
      adicionar(sufijo, trie)
    }

    resultado
  }
}

