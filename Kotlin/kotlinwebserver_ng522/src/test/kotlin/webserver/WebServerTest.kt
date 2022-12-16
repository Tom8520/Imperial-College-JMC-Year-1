package webserver

import org.junit.Test
import kotlin.test.assertEquals

class WebServerTest {

  @Test
  fun `can extract scheme`() {
    assertEquals("http", scheme("http://www.imperial.ac.uk/"))
    assertEquals("https", scheme("https://www.imperial.ac.uk/"))
  }

  @Test
  fun `can extract host`() {
    assertEquals("www.imperial.ac.uk", host("http://www.imperial.ac.uk/"))
    assertEquals("www.imperial.ac.uk", host("https://www.imperial.ac.uk/"))
    assertEquals("www.imperial.ac.uk", host("https://www.imperial.ac.uk/computing"))
  }

  @Test
  fun `can extract path`() {
    assertEquals("/", path("http://www.imperial.ac.uk/"))
    assertEquals("/", path("https://www.imperial.ac.uk/"))
    assertEquals("/computing", path("https://www.imperial.ac.uk/computing"))
    assertEquals("/computing/programming", path("https://www.imperial.ac.uk/computing/programming"))
    assertEquals("/computing", path("https://www.imperial.ac.uk/computing?q=abc"))
  }

  @Test
  fun `can extract query params`() {
    assertEquals(listOf(Pair("q", "xxx")), queryParams("http://www.imperial.ac.uk/?q=xxx"))
    assertEquals(listOf(Pair("q", "xxx"), Pair("rr", "zzz")), queryParams("http://www.imperial.ac.uk/?q=xxx&rr=zzz"))
  }

  @Test
  fun `when no query params in url, empty list is extracted`() {
    assertEquals(listOf(), queryParams("http://www.imperial.ac.uk/"))
  }

// ***** Tests for Handlers *****

  @Test
  fun `says hello world`() {
    val request = Request("http://www.imperial.ac.uk/say-hello")
    assertEquals("Hello, World!", helloHandler(request).body)
  }

  @Test
  fun `can be customised with particular name`() {
    val request = Request("http://www.imperial.ac.uk/say-hello?name=Fred")
    assertEquals("Hello, Fred!", helloHandler(request).body)
  }

  @Test
  fun `can process multiple params`() {
    val request = Request("http://www.imperial.ac.uk/say-hello?name=Fred&style=shouting")
    assertEquals("HELLO, FRED!", helloHandler(request).body)
  }

// ***** Tests for Routing *****

  @Test
  fun `can route to hello handler`() {
    val request = Request("http://www.imperial.ac.uk/say-hello?name=Fred")
    assertEquals("Hello, Fred!", route(request).body)
  }

  @Test
  fun `can route to homepage handler`() {
    assertEquals("This is Imperial.", route(Request("http://www.imperial.ac.uk/")).body)
    assertEquals("This is DoC.", route(Request("http://www.imperial.ac.uk/computing")).body)
  }

  @Test
  fun `gives 404 when no matching route`() {
    assertEquals(Status.NOT_FOUND, route(Request("http://www.imperial.ac.uk/not-here")).status)
  }

//  ***** Tests for the Extensions *****

// *** More flexible routing ***

  @Test
  fun `calling configureRoutes() returns app which can handle requests`() {
    val app = configureRoutes(
      mapOf(
        "/say-hello" to ::helloHandler,
        "/" to ::homepageHandler,
        "/computing" to ::computingHandler
      )
    )

    assertEquals("This is Imperial.", app(Request("http://www.imperial.ac.uk/")).body)
    assertEquals("This is DoC.", app(Request("http://www.imperial.ac.uk/computing")).body)
  }

//  *** Filters ***

  @Test
  fun `filter prevents access to protected resources `() {
    val app = configureRoutes(
      mapOf(
        "/say-hello" to ::helloHandler,
        "/" to ::homepageHandler,
        "/computing" to ::computingHandler,
        "/exam-marks" to requireToken("password1", ::marksHandler)
      )
    )

    val request = Request("http://www.imperial.ac.uk/exam-marks")
    assertEquals(Status.FORBIDDEN, app(request).status)
  }

  @Test
  fun `filter allows access to protected resources with token`() {
    val app = configureRoutes(
      mapOf(
        "/say-hello" to ::helloHandler,
        "/" to ::homepageHandler,
        "/computing" to ::computingHandler,
        "/exam-marks" to requireToken("password1", ::marksHandler)
      )
    )

    val request = Request("http://www.imperial.ac.uk/exam-marks", "password1")
    assertEquals(Status.OK, app(request).status)
    assertEquals("This is very secret.", app(request).body)
  }
}
