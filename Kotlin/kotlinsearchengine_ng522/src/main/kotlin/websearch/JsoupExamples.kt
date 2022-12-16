package websearch

import org.jsoup.Jsoup.connect
import org.jsoup.Jsoup.parse
import org.jsoup.nodes.Document
import org.jsoup.select.Elements

fun main() {
  val htmlString =
    """<html>
         <head>
           <title>A Simple HTML Document</title>
         </head>
           <body>
             <p>This is a very simple <a href="https://en.wikipedia.org/wiki/HTML">HTML</a> document.</p>
             <p>It only has   two    paragraphs.</p>
           </body>
        </html>"""

  // parse a string into a Document using Jsoup.parse()
  val htmlDocument: Document = parse(htmlString)

  // get all the text from the document
  println(htmlDocument.text())

  // extract all the <a >...</a> tags (which create links)
  val aTags: Elements = htmlDocument.getElementsByTag("a")
  println(aTags)

  // get the value of the href attribute (where the link points to)
  val firstTag = aTags.first()

  // aTags.first() could return null, so we have to check that before using it.
  if (firstTag != null) {
    val link: String = firstTag.attr("href")
    println(link)
  }

  // A different approach to dealing with the nullable value is to use the "safe-call" operator ?.
  // This says, if firstTag is null, don't call .attr(), just return null.
  println(firstTag?.attr("href"))

  // You can also use the "Elvis" operator ?: to use a specific value if the safe-call returns null
  println(firstTag?.attr("href") ?: "")

  // do the same thing using a CSS selector
  val selection: Elements = htmlDocument.select("a[href]")
  val firstResult = selection.first()

  println(firstResult?.attr("href"))
  println(firstResult?.attr("href") ?: "")

  // download a document from the internet using Jsoup.connect()
  val downloadedDocument = connect("https://kotlinlang.org/").get()
  println(downloadedDocument.body())
}
