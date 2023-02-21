package museumvisit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.junit.Test;

public class MuseumTest {

  @Test
  public void simpleMuseumTopology() {
    Museum museum = Museum.buildSimpleMuseum();
    assertTrue(museum.getEntrance() instanceof Entrance);
    assertTrue(museum.getExit() instanceof Exit);
    museum.getEntrance().enter();

    assertTrue(museum.getEntrance().getExitTurnstiles().size() == 1);
    assertTrue(
        museum.getEntrance().getExitTurnstiles().get(0).getDestinationRoom()
            instanceof ExhibitionRoom);

    Optional<MuseumSite> exitSite =
        museum.getEntrance().getExitTurnstiles().get(0).passToNextRoom();
    assertTrue(((ExhibitionRoom) exitSite.get()).getCapacity() == 10);

    assertTrue(exitSite.get().getExitTurnstiles().size() == 1);
    assertTrue(exitSite.get().getExitTurnstiles().get(0).getDestinationRoom() instanceof Exit);
    assertTrue(museum.getExit().getExitTurnstiles().isEmpty());
  }

  @Test
  public void loopyMuseumTopology() {
    Museum museum = Museum.buildLoopyMuseum();
    assertTrue(museum.getEntrance() instanceof Entrance);
    assertTrue(museum.getExit() instanceof Exit);
    museum.getEntrance().enter();

    assertTrue(museum.getEntrance().getExitTurnstiles().size() == 1);
    assertTrue(
        museum.getEntrance().getExitTurnstiles().get(0).getDestinationRoom()
            instanceof ExhibitionRoom);

    Optional<MuseumSite> venomExhibitionRoom =
        museum.getEntrance().getExitTurnstiles().get(0).passToNextRoom();
    assertTrue(((ExhibitionRoom) venomExhibitionRoom.get()).getCapacity() == 10);

    // Venom Room has double exits
    assertTrue(venomExhibitionRoom.get().getExitTurnstiles().size() == 2);

    // Dummy visitors to avoid assertion
    venomExhibitionRoom.get().enter();
    venomExhibitionRoom.get().enter();
    Optional<MuseumSite> whalesExhibitionRoom;
    Optional<MuseumSite> exitSite;
    if (venomExhibitionRoom.get().getExitTurnstiles().get(0).passToNextRoom().get()
        instanceof ExhibitionRoom) {
      whalesExhibitionRoom = venomExhibitionRoom.get().getExitTurnstiles().get(0).passToNextRoom();
      exitSite = venomExhibitionRoom.get().getExitTurnstiles().get(1).passToNextRoom();
    } else {
      whalesExhibitionRoom = venomExhibitionRoom.get().getExitTurnstiles().get(1).passToNextRoom();
      exitSite = venomExhibitionRoom.get().getExitTurnstiles().get(0).passToNextRoom();
    }
    assertTrue(whalesExhibitionRoom.get().getExitTurnstiles().size() == 1);
    assertTrue(((ExhibitionRoom) whalesExhibitionRoom.get()).getCapacity() == 10);

    assertTrue(
        whalesExhibitionRoom.get().getExitTurnstiles().get(0).getDestinationRoom()
            instanceof ExhibitionRoom);
    assertTrue(exitSite.get() instanceof Exit);
    assertTrue(museum.getExit().getExitTurnstiles().isEmpty());
  }

  @Test
  public void theMuseumsTopologiesAreConnected() {
    theTopologyIsConnected(Museum.buildSimpleMuseum());
    theTopologyIsConnected(Museum.buildLoopyMuseum());
  }

  @Test(timeout = 5000)
  public void aVisitToTheSimpleMuseumMostLikelyTerminates() {
    final Museum museum = Museum.buildSimpleMuseum();
    aVisitMostLikelyTerminates(museum);
  }

  @Test(timeout = 5000)
  public void aVisitToTheLoopyMuseumMostLikelyTerminates() {
    final Museum museum = Museum.buildLoopyMuseum();
    aVisitMostLikelyTerminates(museum);
  }

  private void aVisitMostLikelyTerminates(Museum museum) {
    final int numberOfVisitors = 50;

    List<Thread> visitors = new ArrayList<>();
    IntStream.range(0, numberOfVisitors)
        .sequential()
        .forEach(
            i -> {
              Thread visitorThread = new Thread(new Visitor("Vis" + i, museum.getEntrance()));
              visitors.add(visitorThread);
              visitorThread.start();
            });

    // Wait for all visitors to complete their visit
    visitors.forEach(
        v -> {
          try {
            v.join();
          } catch (InterruptedException e) {
          }
        });

    Set<MuseumSite> reachableSitesFromEntrance = reachableSitesFrom(museum.getEntrance());
    reachableSitesFromEntrance.stream()
        .forEach(
            room -> {
              if (room.equals(museum.getExit())) {
                assertEquals(numberOfVisitors, room.getOccupancy());
              } else {
                assertEquals(0, room.getOccupancy());
              }
            });
  }

  private void theTopologyIsConnected(Museum museum) {
    Set<MuseumSite> reachableFromEntrance = reachableSitesFrom(museum.getEntrance());

    for (MuseumSite reachableSite : reachableFromEntrance) {
      assertTrue(reachableSitesFrom(reachableSite).contains(museum.getExit()));
    }
  }

  private Set<MuseumSite> reachableSitesFrom(MuseumSite originSite) {
    List<MuseumSite> sitesStillToExplore = new ArrayList<>();
    sitesStillToExplore.add(originSite);
    Set<MuseumSite> reachableFromTheEntrance = new HashSet<>();

    while (!sitesStillToExplore.isEmpty()) {
      final MuseumSite nextSiteToExplore = sitesStillToExplore.get(0);
      List<MuseumSite> newReachableRooms =
          nextSiteToExplore.getExitTurnstiles().stream()
              .map(t -> t.getDestinationRoom())
              .collect(Collectors.toList());

      List<MuseumSite> newReachableRoomsNotExploredYet =
          newReachableRooms.stream()
              .filter(r -> !reachableFromTheEntrance.contains(r))
              .collect(Collectors.toList());
      sitesStillToExplore.addAll(newReachableRoomsNotExploredYet);

      reachableFromTheEntrance.add(nextSiteToExplore);
      sitesStillToExplore.remove(0);
    }
    return reachableFromTheEntrance;
  }
}
