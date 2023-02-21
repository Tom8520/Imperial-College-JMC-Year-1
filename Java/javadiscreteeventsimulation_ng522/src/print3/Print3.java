package print3;

import simulation.Simulation;

public class Print3 extends Simulation<Print3> {

  public static void main(String[] args) {
    Print3 p = new Print3();
    p.schedule(new Print(1), 7.2);
    p.schedule(new Print(2), 11.6);
    p.schedule(new Print(3), 2.9);
    p.simulate();
  }

  @Override
  protected boolean stop() {
    return false;
  }

  @Override
  protected Print3 getState() {
    return this;
  }
}
