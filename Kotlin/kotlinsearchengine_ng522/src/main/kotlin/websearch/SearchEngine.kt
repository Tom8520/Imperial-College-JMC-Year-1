package websearch

class SearchEngine(private var corpus: Map<URL, WebPage>) {
  private var results = mutableMapOf<String, List<SearchResult>>()

  fun compileIndex() {
    val pairs = corpus.toList().fold(emptyList<Pair<URL, String>>()) { x, y -> x + y.second.extractWords().map { y.first to it } }
    val groups = pairs.groupBy { it.second }

    groups.forEach {
      results [it.key] = rank(it.value).sortedBy { -it.numRefs }
    }
  }

  private fun rank(pairs: List<Pair<URL, String>>): List<SearchResult> {
    return pairs.map { SearchResult(it.first, 1) }.sortedBy { it.url.url }.fold(emptyList<SearchResult>()) { x, y ->
      if (x.isNotEmpty() && x.last().url == y.url)x.dropLast(1) + SearchResult(y.url, x.last().numRefs + 1)
      else x + y
    }
  }

  fun searchFor(query: String): SearchResultsSummary {
    if (!results.containsKey(query))return SearchResultsSummary(query, emptyList())
    return SearchResultsSummary(query, results [query]!!)
  }
}

class SearchResult(var url: URL, var numRefs: Int)

class SearchResultsSummary(val query: String, val results: List<SearchResult>) {
  override fun toString(): String {
    var s = "Results for \"$query\":\n"
    results.forEach {
      s += "\t${it.url} - ${it.numRefs} references\n"
    }
    return s.dropLast(1)
  }
}
