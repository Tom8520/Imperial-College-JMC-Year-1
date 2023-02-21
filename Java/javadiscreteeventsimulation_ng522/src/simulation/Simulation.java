package simulation;

import java.util.PriorityQueue;
import java.util.Queue;

public abstract class Simulation<S> {

  protected Queue<ScheduledEvent<S>> eventQueue = new PriorityQueue<>();
  private double currentTime = 0;

  protected boolean stop() {
    return false;
  }

  public void schedule(Event<S> e, double offset) {
    eventQueue.add(new ScheduledEvent<S>(e, currentTime + offset));
  }

  protected abstract S getState();

  public void simulate() {
    while (!eventQueue.isEmpty()) {
      ScheduledEvent<S> event = eventQueue.poll();

      currentTime = event.getScheduledTime();

      if ( stop() )return;
      event.invoke(getState());
    }
  }

  public double getCurrentTime() {
    return currentTime;
  }
}
