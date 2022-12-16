package webserver

// provided files

class Request(val url: String, val authToken: String = "")

class Response(val status: Status, val body: String = "")

enum class Status(val code: Int) {
  OK(200),
  FORBIDDEN(403),
  NOT_FOUND(404)
}
