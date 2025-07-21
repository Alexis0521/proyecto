object PruebaArbolSufijos extends App {
  import ArbolSufijos._
  /*
    // Secuencias de prueba
    val secuencias = Seq("hola".toSeq, "sol".toSeq)
  
    // Construimos el árbol de sufijos
    val arbol = arbolDeSufijos(secuencias)
  
    // Pruebas de pertenencia
    val pruebas = Seq("hola", "ola", "la", "a", "sol", "ol", "l", "no")
  
    println("Resultados de búsqueda:")
    pruebas.foreach { palabra =>
      val perteneceAlArbol = pertenece(palabra.toSeq, arbol)
      println(f"$palabra%-5s -> ${if (perteneceAlArbol) "Sí" else "No"}")
    }
  */

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

