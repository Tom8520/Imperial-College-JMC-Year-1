package journeyplan

// Add your code for the route planner in this file.

class SubwayMap(val segments: List<Segment>) {

  fun routesFrom(origin: Station, destination: Station, optimisingFor: (Route) -> Int = Route::duration): List<Route> {
    val routes = routesFromHelper(origin, destination, emptyList()).sortedBy(optimisingFor)
    return routes.filter { it.segments.any { x -> !x.start.closed && !x.end.closed } }
  }

  private fun routesFromHelper(origin: Station, destination: Station, visited: List<Station>): List<Route> {
    if (origin == destination)return listOf(Route(emptyList()))
    var routes: List<Route> = emptyList()
    val newVisited = visited + origin
    for (seg in segments) {
      if (seg.start == origin && !(seg.end in visited) && !seg.line.suspended) {
        var newRoutes = routesFromHelper(seg.end, destination, newVisited).map { Route(listOf(seg) + it.segments) }
        // newRoutes.forEach { it.apply { this.segments = listOf(seg) + this.segments } }
        routes += newRoutes
      }
    }
    return routes
  }
}

fun londonUnderground(): SubwayMap {
  val SK = Station("South Kensington")
  val GP = Station("Green Park")
  val BS = Station("Bond Street")
  val NHG = Station("Notting Hill Gate")
  val OS = Station("Oxford Circus")
  val V = Station("Victoria")

  val Pic = Line("Piccadilly")
  val Cir = Line("Circle")
  val Vic = Line("Victoria")
  val Cen = Line("Central")
  val Jub = Line("Jubilee")
  val Dis = Line("District")

  val oneWaySegments = listOf(
    Segment(NHG, BS, Cen, 5),
    Segment(NHG, SK, Cir, 4),
    Segment(NHG, SK, Dis, 5),
    Segment(SK, GP, Pic, 6),
    Segment(GP, V, Vic, 8),
    Segment(V, SK, Cir, 3),
    Segment(V, SK, Dis, 4),
    Segment(GP, BS, Jub, 10),
    Segment(GP, OS, Vic, 7),
    Segment(BS, OS, Cen, 8)
  )

  return SubwayMap(oneWaySegments + oneWaySegments.map { Segment(it.end, it.start, it.line, it.time) })
}

class Route(var segments: List<Segment>) {

  init {
    segments = segments.fold(emptyList()) { x, y ->
      if (x.isNotEmpty() && x.last().line == y.line) {
        x.dropLast(1) + Segment(x.last().start, y.end, y.line, x.last().time + y.time)
      } else x + y
    }
  }

  override fun toString(): String {
    var s = "${segments [0].start} to ${segments.last().end} - ${duration()} minutes, ${numChanges()} changes\n"
    for (seg in segments) {
      s += " - ${seg.start} to ${seg.end} by ${seg.line}\n"
    }
    return s.dropLast(1)
  }

  fun numChanges(): Int {
    var n = 0
    var prev = segments [0].line
    for (seg in segments) {
      if (seg.line != prev)n++
      prev = seg.line
    }
    return n
  }

  fun duration(): Int = segments.fold(0) { x, y -> x + y.time }
}
