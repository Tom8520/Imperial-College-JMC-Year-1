package museumvisit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ExhibitionRoomTest {

  @Test
  public void hasAvailability() {
    ExhibitionRoom room = new ExhibitionRoom("room", 3);
    assertEquals(0, room.getOccupancy());
    assertTrue(room.hasAvailability());
    room.enter();
    assertEquals(1, room.getOccupancy());
    assertTrue(room.hasAvailability());
    room.enter();
    assertEquals(2, room.getOccupancy());
    assertTrue(room.hasAvailability());
    room.enter();
    assertEquals(3, room.getOccupancy());
    assertFalse(room.hasAvailability());

    room.exit();
    assertEquals(2, room.getOccupancy());
    assertTrue(room.hasAvailability());

    room.exit();
    assertEquals(1, room.getOccupancy());
    assertTrue(room.hasAvailability());

    room.exit();
    assertEquals(0, room.getOccupancy());
    assertTrue(room.hasAvailability());
  }
}
