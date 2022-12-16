package websearch

import org.jsoup.Jsoup.connect
import org.jsoup.nodes.Document

class URL(var url: String) {
  override fun toString(): String {
    return url
  }

  override fun equals(other: Any?): Boolean {
    return (other is URL) && (url == other.url)
  }

  fun download(): WebPage {
    return WebPage(connect(url).get())
  }
}

class WebPage(private var document: Document) {
  fun extractWords(): List<String> {
    return document.text().lowercase().replace(".", "").replace(",", "").split(" ")
  }

  fun extractLinks(): List<URL> {
    var links = emptyList<URL>()
    document.getElementsByTag("a").forEach {
      if (it.attr("href").startsWith("http")) {
        links += URL(it.attr("href"))
      }
    }
    return links
  }
}
