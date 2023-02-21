package museumvisit;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.IntStream;

public class Museum {

  private final Entrance entrance;
  private final Exit exit;
  private final Set<MuseumSite> sites;

  public Museum(Entrance entrance, Exit exit, Set<MuseumSite> sites) {
    this.entrance = entrance;
    this.exit = exit;
    this.sites = sites;
  }

  public static void main(String[] args) {
    final int numberOfVisitors = 100; // Your solution has to work with any
    // number of visitors
    final Museum museum = buildLoopyMuseum();

    // create the threads for the visitors and get them moving

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

    // Checking no one is left behind
    if (museum.getExit().getOccupancy() == numberOfVisitors) {
      System.out.println("\nAll the visitors reached the exit\n");
    } else {
      System.out.println(
          "\n"
              + (numberOfVisitors - museum.getExit().getOccupancy())
              + " visitors did not reach the exit. Where are they?\n");
    }

    System.out.println("Occupancy status for each room (should all be zero, but the exit site):");
    museum
        .getSites()
        .forEach(
            s -> {
              System.out.println("Site " + s.getName() + " final occupancy: " + s.getOccupancy());
            });
  }

  public static Museum buildSimpleMuseum() {
    Entrance entrance = new Entrance();
    Exit exit = new Exit();
    MuseumSite exe = new ExhibitionRoom("Exhibition Room", 10);

    Turnstile entToExe = new Turnstile(entrance, exe);
    Turnstile exeToExit = new Turnstile(exe, exit);

    return new Museum(entrance, exit, Set.of(entrance, exe, exit));
  }

  public static Museum buildLoopyMuseum() {
    Entrance entrance = new Entrance();
    Exit exit = new Exit();
    MuseumSite venom = new ExhibitionRoom("Venom KillerAndCure Room", 10);
    MuseumSite whales = new ExhibitionRoom("Whales Exhibition Room", 10);

    Turnstile entToVen = new Turnstile(entrance, venom);
    Turnstile venToWha = new Turnstile(venom, whales);
    Turnstile whaToVen = new Turnstile(whales, venom);
    Turnstile venToExit = new Turnstile(venom, exit);

    return new Museum(entrance, exit, Set.of(entrance, venom, whales, exit));
  }

  public Entrance getEntrance() {
    return entrance;
  }

  public Exit getExit() {
    return exit;
  }

  public Set<MuseumSite> getSites() {
    return sites;
  }
}
