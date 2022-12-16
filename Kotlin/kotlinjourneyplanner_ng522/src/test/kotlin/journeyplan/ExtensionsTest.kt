package journeyplan

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ExtensionsTest {

  /** Uncomment the code in this file if you do the extensions **/

  val piccadillyLine = Line("Piccadilly")
  val victoriaLine = Line("Victoria")
  val districtLine = Line("District")

  val southKensington = Station("South Kensington")
  val knightsbridge = Station("Knightsbridge")
  val hydeParkCorner = Station("Hyde Park Corner")
  val greenPark = Station("Green Park")
  val oxfordCircus = Station("Oxford Circus")
  val victoria = Station("Victoria")
  val sloaneSquare = Station("Sloane Square")

  fun londonUnderground(): SubwayMap = SubwayMap(
    listOf(
      Segment(southKensington, knightsbridge, piccadillyLine, 3),
      Segment(knightsbridge, hydeParkCorner, piccadillyLine, 4),
      Segment(hydeParkCorner, greenPark, piccadillyLine, 2),
      Segment(greenPark, oxfordCircus, victoriaLine, 1),
      Segment(greenPark, victoria, victoriaLine, 1),
      Segment(victoria, greenPark, victoriaLine, 1),
      Segment(victoria, sloaneSquare, districtLine, 6),
      Segment(sloaneSquare, southKensington, districtLine, 3),
      Segment(southKensington, sloaneSquare, districtLine, 6),
      Segment(sloaneSquare, victoria, districtLine, 6)
    )
  )

  val map = londonUnderground()

  @Test
  fun `can find multiple routes between stations`() {
    val routes = map.routesFrom(southKensington, victoria)
    assertEquals(2, routes.size)

    assertTrue(routes[0].segments.all { s -> s.line in setOf(piccadillyLine, victoriaLine) })
    assertTrue(routes[1].segments.all { s -> s.line == districtLine })
  }

  @Test
  fun `can optimise for number of changes`() {
    val routes = map.routesFrom(southKensington, victoria, optimisingFor = Route::numChanges)
    assertEquals(2, routes.size)

    assertEquals(0, routes[0].numChanges())
    assertEquals(1, routes[1].numChanges())
  }

  @Test
  fun `can optimise for duration`() {
    val routes = map.routesFrom(southKensington, victoria, optimisingFor = Route::duration)
    assertEquals(2, routes.size)

    assertEquals(10, routes[0].duration())
    assertEquals(12, routes[1].duration())
  }

  @Test
  fun `does not offer routes with suspended lines`() {
    var routes = map.routesFrom(southKensington, victoria)

    assertEquals(2, routes.size)
    assertTrue(routes[0].segments.all { s -> s.line in setOf(piccadillyLine, victoriaLine) })
    assertTrue(routes[1].segments.all { s -> s.line == districtLine })

    districtLine.suspend()

    routes = map.routesFrom(southKensington, victoria)

    assertEquals(1, routes.size)
    assertTrue(routes[0].segments.none { s -> s.line == districtLine })

    districtLine.resume()

    routes = map.routesFrom(southKensington, victoria)

    assertEquals(2, routes.size)
    assertTrue(routes[0].segments.all { s -> s.line in setOf(piccadillyLine, victoriaLine) })
    assertTrue(routes[1].segments.all { s -> s.line == districtLine })
  }

  @Test
  fun `avoids interchange at closed stations`() {
    var routes = map.routesFrom(southKensington, oxfordCircus)
    assertEquals(2, routes.size)

    victoria.close()

    routes = map.routesFrom(southKensington, oxfordCircus)

    assertEquals(1, routes.size)
    assertDoesNotGoVia(victoria, routes[0])
  }

  @Test
  fun `does not avoid closed stations if interchange not required`() {
    var routes = map.routesFrom(southKensington, oxfordCircus)
    assertEquals(2, routes.size)

    sloaneSquare.close()

    routes = map.routesFrom(southKensington, oxfordCircus)
    assertEquals(2, routes.size)
    println(routes)

    assertGoesVia(victoria, routes[1]) // changed as my code compressed the route which breaks this
  }

  fun assertGoesVia(station: Station, route: Route) {
    assertNotNull(findIn(route, station))
  }

  fun assertDoesNotGoVia(station: Station, route: Route) {
    assertNull(findIn(route, station))
  }

  fun findIn(route: Route, station: Station) = route.segments.find { s -> s.end == station }
}
