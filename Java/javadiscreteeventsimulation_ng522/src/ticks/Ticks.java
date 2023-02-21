package ticks;

import simulation.Simulation;

public class Ticks extends Simulation<Ticks> {

  private final double runTime;

  public Ticks(double runTime) {
    this.runTime = runTime;
    schedule(new TickEvent(), 1);
  }

  public static void main(String[] args) {
    double runTime = Double.parseDouble(args[0]);

    Ticks t = new Ticks(runTime);
    t.simulate();
  }

  @Override
  protected boolean stop() {
    return getCurrentTime() >= runTime;
  }

  @Override
  protected Ticks getState() {
    return this;
  }
}
