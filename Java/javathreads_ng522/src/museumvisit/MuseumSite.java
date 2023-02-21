package museumvisit;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class MuseumSite {

  protected final String name;
  protected int occupancy;
  protected List<Turnstile> exitTurnstiles;

  public MuseumSite(String name) {
    this.name = name;
    this.occupancy = 0;
    this.exitTurnstiles = new ArrayList<>();
  }

  boolean hasAvailability() {
    return true;
  }

  public void enter() {
    occupancy++;
  }

  public void exit() {
    assert occupancy > 0;
    occupancy--;
  }

  public void addExitTurnstile(Turnstile turnstile) {
    exitTurnstiles.add(turnstile);
  }

  public List<Turnstile> getExitTurnstiles() {
    return new ArrayList<>(exitTurnstiles);
  }

  public String getName() {
    return name;
  }

  public int getOccupancy() {
    return occupancy;
  }

  @Override
  public String toString() {
    return "Site[" + name + "]";
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof MuseumSite) {
      return name.equals(((MuseumSite) obj).name);
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(getClass(), name);
  }
}
