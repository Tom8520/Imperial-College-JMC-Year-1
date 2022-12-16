package websearch

class WebCrawler(private val url: URL) {
  private val MAX_DOWNLOAD = 10
  private var results = mutableMapOf<URL, WebPage>()
  private var used = mutableListOf<URL>()

  fun run(downloaded: Int = 0, link: URL = url): Int {
    if (downloaded == MAX_DOWNLOAD || link in used)return downloaded
    used.add(link)

    try {
      val page = link.download()

      results[link] = page
      var newDownloaded = downloaded + 1

      page.extractLinks().forEach {
        newDownloaded = run(newDownloaded, it)
      }

      return newDownloaded
    } catch (e: Throwable) {
      return downloaded
    }
  }

  fun dump(): Map<URL, WebPage> = results.toMap()
}

fun main() {
  val crawler = WebCrawler(URL("http://www.bbc.co.uk"))
  crawler.run()
  val searchEngine = SearchEngine(crawler.dump())
  searchEngine.compileIndex()
  println(searchEngine.searchFor("news"))
}
