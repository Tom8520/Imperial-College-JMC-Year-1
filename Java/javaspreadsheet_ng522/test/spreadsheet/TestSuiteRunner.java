package spreadsheet;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class TestSuiteRunner {

  public static void main(String[] args) {
    Result result =
        JUnitCore.runClasses(
            spreadsheet.TestEvaluation.class,
            spreadsheet.TestFindCellReferences.class,
            spreadsheet.TestMain.class,
            spreadsheet.TestParser.class,
            spreadsheet.TestSpreadsheet.class);
    for (Failure failure : result.getFailures()) {
      System.out.println(failure.toString());
    }
  }
}
