package journeyplan

import org.junit.Test
import kotlin.test.assertEquals

class TravelModelTest {

  @Test
  fun `printing stations shows their names`() {
    assertEquals("South Kensington", Station("South Kensington").toString())
    assertEquals("Knightsbridge", Station("Knightsbridge").toString())
  }

  @Test
  fun `printing lines shows their names`() {
    assertEquals("District Line", Line("District").toString())
    assertEquals("Circle Line", Line("Circle").toString())
  }
}
