package ssq;

import java.util.Random;
import simulation.Simulation;

public class SingleServerQueue extends Simulation<SingleServerQueue> {

  private static final double SERVICE_TIME = 0.25;
  private final double runTime;
  private final Random arrivalGenerator;
  private int population = 0;
  private double meanQueueLength = 0;
  private double lastMeanUpdateTime = 0;

  public SingleServerQueue(long seed, double runTime) {
    arrivalGenerator = new Random(seed);
    this.runTime = runTime;
    scheduleNextArrival();
  }

  public static void main(String[] args) {
    long seed = Long.parseLong(args[0]);
    double runTime = Double.parseDouble(args[1]);

    SingleServerQueue ssq = new SingleServerQueue(seed, runTime);
    ssq.simulate();

    double mean = ssq.getMeanQueueLength();
    System.out.println("SIMULATION COMPLETE - the mean queue length was " + mean);
  }

  @Override
  public boolean stop() {
    return getCurrentTime() > runTime;
  }

  @Override
  public SingleServerQueue getState() {
    return this;
  }

  public void scheduleNextArrival() {
    double arrivalTime = arrivalGenerator.nextDouble();

    if (population == 1 ) {
      scheduleNextDeparture();
    }

    schedule(new SSQArrival(), arrivalTime);
  }

  public void scheduleNextDeparture() {
    if (population > 0) {
      schedule(new SSQDeparture(), SERVICE_TIME);
    }
  }

  public int getPopulation() {
    return population;
  }

  public void incrementPopulation() {
    population++;
  }

  public void decrementPopulation() {
    population--;
  }

  public void updateMeanQueueLength() {
    meanQueueLength += (getCurrentTime() - lastMeanUpdateTime) * population;
    lastMeanUpdateTime = getCurrentTime();
  }

  public double getMeanQueueLength() {
    return (meanQueueLength + population * (runTime - lastMeanUpdateTime)) / runTime;
  }
}
