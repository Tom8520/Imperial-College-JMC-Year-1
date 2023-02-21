package ssq;

import simulation.Event;

public class SSQDeparture implements Event<SingleServerQueue> {

  public void invoke(SingleServerQueue ssq) {
    ssq.updateMeanQueueLength();
    ssq.decrementPopulation();

    double time = ssq.getCurrentTime();
    int population = ssq.getPopulation();

    System.out.println("Departure at " + time + ", new population = " + population);

    ssq.scheduleNextDeparture();
  }
}
