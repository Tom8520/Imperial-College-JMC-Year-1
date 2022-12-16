package journeyplan

// Add your code for modelling public transport networks in this file.

class Station(val name: String) {
  var closed = false
  override fun toString(): String {
    return name
  }

  fun close() {
    closed = true
  }

  fun open() {
    closed = false
  }
}

class Line(val name: String) {
  var suspended = false
  override fun toString(): String {
    return "$name Line"
  }

  fun suspend() {
    suspended = true
  }

  fun resume() {
    suspended = false
  }
}

class Segment(var start: Station, var end: Station, var line: Line, var time: Int)
