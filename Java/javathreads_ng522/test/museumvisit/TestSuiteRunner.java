package museumvisit;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class TestSuiteRunner {

  public static void main(String[] args) {
    Result result =
        JUnitCore.runClasses(
            museumvisit.ExhibitionRoomTest.class,
            museumvisit.MuseumTest.class,
            museumvisit.TurnstileTest.class);
    for (Failure failure : result.getFailures()) {
      System.out.println(failure.toString());
    }
  }
}
