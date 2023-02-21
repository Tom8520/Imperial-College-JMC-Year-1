package simulation;

public class ScheduledEvent<S> implements Comparable<ScheduledEvent> {

  private final Event<S> event;
  private final double scheduledTime;

  public ScheduledEvent(Event<S> event, double scheduledTime) {
    this.event = event;
    this.scheduledTime = scheduledTime;
  }

  public int compareTo(ScheduledEvent other) {
    return Double.compare(scheduledTime, other.scheduledTime);
  }

  public double getScheduledTime() {
    return scheduledTime;
  }

  void invoke(S simulation) {
    event.invoke(simulation);
  }
}
