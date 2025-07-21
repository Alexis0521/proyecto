object PruebaArbolSufijos extends App {
  import ArbolSufijos._
  
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

