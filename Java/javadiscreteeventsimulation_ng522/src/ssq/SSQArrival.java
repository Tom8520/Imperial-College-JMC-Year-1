package ssq;

import simulation.Event;

public class SSQArrival implements Event<SingleServerQueue> {

  public void invoke(SingleServerQueue ssq) {
    ssq.updateMeanQueueLength();
    ssq.incrementPopulation();

    double time = ssq.getCurrentTime();
    int population = ssq.getPopulation();

    System.out.println("Arrival at " + time + ", new population = " + population);

    ssq.scheduleNextArrival();
  }
}
