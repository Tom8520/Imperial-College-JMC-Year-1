package websearch

import org.jsoup.Jsoup.parse
import java.io.File

val downloadedWebPages: Map<URL, WebPage> =
  mapOf(
    URL("https://kotlinlang.org/") to loadPage("kotlinlang.html"),
    URL("https://research.google/pubs/pub62/") to loadPage("mapreduce.html"),
    URL("https://wiki.haskell.org/Introduction") to loadPage("haskellwiki.html")
  )
private fun loadPage(filename: String) =
  WebPage(parse(File("src/main/data/$filename").bufferedReader().readLines().joinToString("\n")))
