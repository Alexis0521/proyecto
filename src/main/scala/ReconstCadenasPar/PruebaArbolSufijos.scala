object PruebaArbolSufijos extends App {

  import scala.collection.parallel.CollectionConverters._
  import Oraculo._
  import ArbolSufijos._


  val alfabeto = Seq('a', 'c', 'g', 't')
  
  

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    var k = 1
    var SC = Oraculo.alfabeto.map(Seq(_))
    while (k < n) {
      val SCk2 = for {
        s1 <- SC
        s2 <- SC
      } yield s1 ++ s2

      val SCk2Valid =
        if (SCk2.size >= umbral) SCk2.par.filter(o).toList
        else SCk2.filter(o)

      SCk2Valid.find(_.length == n) match {
        case Some(sol) => return sol
        case None =>
      }
      SC = SCk2Valid
      k = k * 2
    }
    Seq()
  }

  // Impresión estructurada (muy simple) del árbol
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


  val palabras = Seq("hola".toSeq, "sol".toSeq)
  val arbolPrueba = arbolDeSufijos(palabras)

  imprimirTrie(arbolPrueba)

  val pruebas = Seq("hola", "ola", "la", "a", "sol", "ol", "l", "no")

  for (palabra <- pruebas) {
    val chars = palabra.toSeq
    println(f"$palabra%-5s -> ${pertenece(chars, arbolPrueba)}")
  }
}

