package ticks;

import simulation.Event;

public class TickEvent implements Event<Ticks> {

  @Override
  public void invoke(Ticks simulation) {
    double time = simulation.getCurrentTime();
    System.out.println("Tick at: " + time);
    simulation.schedule(new TickEvent(), 1);
  }
}
