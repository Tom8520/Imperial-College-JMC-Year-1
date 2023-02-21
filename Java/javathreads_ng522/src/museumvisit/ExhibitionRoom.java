package museumvisit;

public class ExhibitionRoom extends MuseumSite {

  private int capacity;

  public ExhibitionRoom(String name, int capacity) {
    super(name);
    assert capacity > 0;

    this.capacity = capacity;
  }

  @Override
  public void enter() {
    assert occupancy < capacity;
    super.enter();
  }

  @Override
  boolean hasAvailability() {
    return occupancy < capacity;
  }

  public int getCapacity() {
    return capacity;
  }
}
