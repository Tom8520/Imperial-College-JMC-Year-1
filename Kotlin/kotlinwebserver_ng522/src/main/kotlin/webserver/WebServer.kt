package webserver

// write your web framework code here:

fun scheme(url: String): String =
  url.substringBefore("://")

fun host(url: String): String =
  url.substringAfter("://")
    .substringBefore("/")

fun path(url: String): String =
  "/" + url.substringAfter("://")
    .substringAfter("/")
    .substringBefore("?")

fun queryParams(url: String): List<Pair<String, String>> =
  url.substringAfter("?", "")
    .split("&")
    .filter { x -> x != "" }
    .map { x ->
      val y = x.split("=")
      Pair(y [0], y [1])
    }

// http handlers for a particular website...

// either change the text to "Hello {name}" or make it all caps
// the when in styleHandler could be made to handle an arbitary number of types of style
fun nameHandler(str: String, param: String): String = "Hello, " + param + "!"
fun styleHandler(str: String, param: String): String {
  return when (param) {
    "shouting" -> str.uppercase()
    else -> str
  }
}

// apply each param in order to the starting string "Hello World"
fun helloHandler(request: Request): Response {
  val paramHandlers = mapOf(Pair("name", ::nameHandler), Pair("style", ::styleHandler))
  val params = queryParams(request.url)

  var res = "Hello, World!"

  for (param in params) {
    res = paramHandlers [param.first]!!.invoke(res, param.second)
  }

  return Response(Status.OK, res)
}

// handlers to return given strings for certain paths
fun homepageHandler(request: Request): Response {
  return Response(Status.OK, "This is Imperial.")
}

fun computingHandler(request: Request): Response {
  return Response(Status.OK, "This is DoC.")
}

fun marksHandler(request: Request): Response {
  return Response(Status.OK, "This is very secret.")
}

typealias HttpHandler = (Request) -> Response

fun requireToken(token: String, wrapped: HttpHandler): HttpHandler {
  return { request ->
    if (request.authToken == token)wrapped!!.invoke(request)
    else Response(Status.FORBIDDEN, "Invalid token passed")
  }
}

fun configureRoutes(routeMap: Map<String, HttpHandler>): HttpHandler {
  return { request ->
    val path = path(request.url)

    if (!routeMap.containsKey(path)) Response(Status.NOT_FOUND, "404 page not found")
    routeMap [path]!!.invoke(request)
  }
}

// takes a request and passes it to the correct handler
// very similar to configureRoutes but i hadnt read the rest of the spec when i wrote this one
fun route(request: Request): Response {
  val routeMap = mapOf(
    "/say-hello" to ::helloHandler,
    "/" to ::homepageHandler,
    "/computing" to ::computingHandler
  )

  val path = path(request.url)

  if (!routeMap.containsKey(path)) return Response(Status.NOT_FOUND, "404 page not found")

  return routeMap [path]!!.invoke(request)
}

// sorry for lack of extentions, been busy with arctic lake coding challenge
