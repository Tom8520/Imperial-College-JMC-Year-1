package museumvisit;

import java.util.Optional;

public class Turnstile {

  private final MuseumSite originRoom;
  private final MuseumSite destinationRoom;

  public Turnstile(MuseumSite originRoom, MuseumSite destinationRoom) {
    assert !originRoom.equals(destinationRoom);
    this.originRoom = originRoom;
    this.destinationRoom = destinationRoom;

    originRoom.addExitTurnstile(this);
  }

  public Optional<MuseumSite> passToNextRoom() {
    MuseumSite fstToSync, sndToSync;
    if (originRoom.hashCode() < destinationRoom.hashCode()) {
      fstToSync = originRoom;
      sndToSync = destinationRoom;
    } else {
      fstToSync = destinationRoom;
      sndToSync = originRoom;
    }

    Optional<MuseumSite> nextRoom = Optional.empty();
    synchronized (fstToSync) {
      synchronized (sndToSync) {
        if (destinationRoom.hasAvailability()) {
          originRoom.exit();
          destinationRoom.enter();
          nextRoom = Optional.of(destinationRoom);
        }
      }
    }
    return nextRoom;
  }

  public MuseumSite getOriginRoom() {
    return originRoom;
  }

  public MuseumSite getDestinationRoom() {
    return destinationRoom;
  }
}
