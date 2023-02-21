package spreadsheet;

import static org.junit.Assert.assertEquals;

import common.api.CellLocation;
import org.junit.Test;

public class TestSpreadsheet {

  private static final double TOLERANCE = 0.0001;

  private static final CellLocation A1 = new CellLocation("a1");
  private static final CellLocation B1 = new CellLocation("b1");

  @Test
  public void testEvaluate() throws Exception {
    Spreadsheet spreadsheet = new Spreadsheet();
    assertEquals(spreadsheet.evaluateExpression("1"), 1, TOLERANCE);
    assertEquals(spreadsheet.evaluateExpression("1 + 2"), 3, TOLERANCE);
    assertEquals(spreadsheet.evaluateExpression("2 * 4"), 8, TOLERANCE);
    assertEquals(spreadsheet.evaluateExpression("1 + 2 * 4"), 9, TOLERANCE);
  }

  @Test
  public void testEmptyCellIsZero() throws Exception {
    Spreadsheet spreadsheet = new Spreadsheet();
    assertEquals(spreadsheet.evaluateExpression("a1"), 0, TOLERANCE);

    spreadsheet.setCellExpression(A1, "12");
    assertEquals(spreadsheet.evaluateExpression("b1"), 0, TOLERANCE);
  }

  @Test
  public void testSetCellExpression() throws Exception {
    Spreadsheet spreadsheet = new Spreadsheet();
    spreadsheet.setCellExpression(A1, "12");
    spreadsheet.setCellExpression(B1, "a1 * 2");

    assertEquals(spreadsheet.evaluateExpression("a1"), 12, TOLERANCE);
    assertEquals(spreadsheet.evaluateExpression("b1"), 24, TOLERANCE);
    assertEquals(spreadsheet.evaluateExpression("a1 + b1"), 36, TOLERANCE);
  }

  @Test
  public void testSelfReference() throws Exception {
    Spreadsheet spreadsheet = new Spreadsheet();
    spreadsheet.setCellExpression(A1, "1");
    assertEquals(spreadsheet.evaluateExpression("a1"), 1, TOLERANCE);
    spreadsheet.setCellExpression(A1, "a1 * 2");
    assertEquals(spreadsheet.evaluateExpression("a1"), 2, TOLERANCE);
    spreadsheet.setCellExpression(A1, "a1 * 2");
    assertEquals(spreadsheet.evaluateExpression("a1"), 4, TOLERANCE);
    spreadsheet.setCellExpression(A1, "a1 * 2");
    assertEquals(spreadsheet.evaluateExpression("a1"), 8, TOLERANCE);
    spreadsheet.setCellExpression(A1, "a1 * 2");
    assertEquals(spreadsheet.evaluateExpression("a1"), 16, TOLERANCE);
    spreadsheet.setCellExpression(A1, "a1 * 2");
  }
}
