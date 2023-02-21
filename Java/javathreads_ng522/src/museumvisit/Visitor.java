package museumvisit;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public class Visitor implements Runnable {

  private final String name;
  private MuseumSite currentRoom;

  public Visitor(String name, MuseumSite initialRoom) {
    assert initialRoom instanceof Entrance;
    this.name = name;
    this.currentRoom = initialRoom;
    initialRoom.enter();
  }

  public void run() {
    while (thereAreMoreSitesToVisit()) {
      simulateVisitToCurrentRoom();

      /*
       * 1. pick a random turnstile
       * 2. try to go through it.
       * 2a) if successful (i.e., passToNextRoom() returned a MuseumSite),
       * the returned site will be the next currentRoom to be visited
       * 2b) if unsuccessful (i.e., passToNextRoom() returned an empty Optional),
       * waitSomeTimeBeforeRetrying(), and then retry from step 1.
       */
      while (true) {
        Turnstile exit = pickRandomTurnstile();

        Optional<MuseumSite> nextRoom = exit.passToNextRoom();

        if (nextRoom.isPresent()) {
          currentRoom = nextRoom.get();
          break;
        } else {
          waitSomeTimeBeforeRetrying();
        }
      }
    }
  }

  private boolean thereAreMoreSitesToVisit() {
    return !currentRoom.getExitTurnstiles().isEmpty();
  }

  private Turnstile pickRandomTurnstile() {
    List<Turnstile> exitTurnstiles = currentRoom.getExitTurnstiles();
    assert !exitTurnstiles.isEmpty();

    Collections.shuffle(exitTurnstiles); // Random shuffle of the list of
    // turnstiles
    return exitTurnstiles.stream().findAny().get();
  }

  private void simulateVisitToCurrentRoom() {
    System.out.println("Visitor " + name + ": visiting room: " + currentRoom.getName());
    final int randomVisitTimeInMillis = (int) (Math.random() * 200) + 1;
    // wait between 1  and  200 millis
    try {
      Thread.sleep(randomVisitTimeInMillis);
    } catch (InterruptedException e) {
    }
  }

  private void waitSomeTimeBeforeRetrying() {
    try {
      Thread.sleep(100);
    } catch (InterruptedException e) {
    }
  }

  @Override
  public String toString() {
    return "Visitor " + name;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Visitor) {
      return this.name.equals(((Visitor) obj).name);
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(getClass(), name);
  }
}
