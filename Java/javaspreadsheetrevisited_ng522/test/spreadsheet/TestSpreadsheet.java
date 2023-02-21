package spreadsheet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static spreadsheet.TokensMatcher.matchesTokens;

import common.api.CellLocation;
import org.junit.Test;

public class TestSpreadsheet {

  private static final double TOLERANCE = 0.0001;

  private static final CellLocation A1 = new CellLocation("a1");
  private static final CellLocation B1 = new CellLocation("b1");
  private static final CellLocation A2 = new CellLocation("a2");
  private static final CellLocation B2 = new CellLocation("b2");
  private static final CellLocation C2 = new CellLocation("c2");

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

  // testSelfReference is not present in part 2, because it conflicts with dependency resolution.

  /*
   * PART 2
   */

  /**
   * Tests a few properties of getCellExpression.
   *
   * @throws InvalidSyntaxException if spurious parser errors occur.
   */
  @Test
  public void testGetCellExpression() throws InvalidSyntaxException {
    Spreadsheet spreadsheet = new Spreadsheet();

    // undefined cells should have empty expressions
    assertEquals("", spreadsheet.getCellExpression(A1));

    spreadsheet.setCellExpression(A1, "2 + A2");
    assertThat(spreadsheet.getCellExpression(A1), matchesTokens("( 2 + A2 )"));

    // setting expression to "" should clear it
    spreadsheet.setCellExpression(A1, "");
    assertEquals("", spreadsheet.getCellExpression(A1));
  }

  /**
   * Tests a few properties of getCellDisplay.
   *
   * @throws InvalidSyntaxException if spurious parser errors occur.
   */
  @Test
  public void testGetCellDisplay() throws InvalidSyntaxException {
    Spreadsheet spreadsheet = new Spreadsheet();

    // undefined cells should have empty displays
    assertEquals("", spreadsheet.getCellDisplay(A1));

    // it doesn't matter if the display returns 4 or 4.0; we accept either
    spreadsheet.setCellExpression(A1, "2 + 2");
    assertThat(spreadsheet.getCellDisplay(A1), matchesTokens("4"));

    // setting expression to "" should clear it
    spreadsheet.setCellExpression(A1, "");
    assertEquals("", spreadsheet.getCellDisplay(A1));
  }

  /**
   * Tests that the cycle detection added in part 2 is exposed properly by the spreadsheet. As we
   * don't require any particular cycle detection mechanism, this just checks that there are no
   * unexpected exceptions and the rest of the spreadsheet appears stable.
   *
   * @throws InvalidSyntaxException if any spurious parser failures happen.
   */
  @Test
  public void testSelfReferenceAfterCycleDetection() throws InvalidSyntaxException {
    Spreadsheet spreadsheet = new Spreadsheet();

    spreadsheet.setCellExpression(A1, "1");
    assertEquals(1, spreadsheet.getCellValue(A1), TOLERANCE);

    // direct cycle with B1 depending (B1 shouldn't change)
    spreadsheet.setCellExpression(B1, "a1 + 10");
    spreadsheet.setCellExpression(A1, "a1 * 2");
    assertThat(spreadsheet.getCellExpression(B1), matchesTokens("(a1 + 10)"));

    // indirect cycle on B1 (A1 shouldn't change)
    spreadsheet.setCellExpression(B1, "");
    spreadsheet.setCellExpression(A1, "b1 * 2");
    assertThat(spreadsheet.getCellExpression(A1), matchesTokens("(b1 * 2)"));
    spreadsheet.setCellExpression(B1, "a1 / 2");
    assertThat(spreadsheet.getCellExpression(A1), matchesTokens("(b1 * 2)"));
  }

  /**
   * Tests that the example of uncreated spreadsheet dependencies works properly.
   *
   * @throws InvalidSyntaxException if any spurious parser failures happen.
   */
  @Test
  public void testUncreatedDependencyUpdates() throws InvalidSyntaxException {
    Spreadsheet spreadsheet = new Spreadsheet();

    spreadsheet.setCellExpression(A1, "A2 + B2 * C2");
    spreadsheet.setCellExpression(A2, "10.0");
    spreadsheet.setCellExpression(B2, "4.0");
    spreadsheet.setCellExpression(C2, "B2 * 2.0");

    assertEquals(42.0, spreadsheet.getCellValue(A1), TOLERANCE);
    assertEquals(10.0, spreadsheet.getCellValue(A2), TOLERANCE);
    assertEquals(4.0, spreadsheet.getCellValue(B2), TOLERANCE);
    assertEquals(8.0, spreadsheet.getCellValue(C2), TOLERANCE);
  }

  /**
   * Tests that dependencies on existing cells update properly.
   *
   * @throws InvalidSyntaxException if any spurious parser failures happen.
   */
  @Test
  public void testExistingDependencies() throws InvalidSyntaxException {
    Spreadsheet spreadsheet = new Spreadsheet();

    spreadsheet.setCellExpression(A1, "8.0");
    spreadsheet.setCellExpression(A2, "A1");
    spreadsheet.setCellExpression(B2, "A1 + A2");
    spreadsheet.setCellExpression(C2, "A1 + A2 + B2");

    // Make sure the initial values are right.
    assertEquals(8.0, spreadsheet.getCellValue(A1), TOLERANCE);
    assertEquals(8.0, spreadsheet.getCellValue(A2), TOLERANCE);
    assertEquals(16.0, spreadsheet.getCellValue(B2), TOLERANCE);
    assertEquals(32.0, spreadsheet.getCellValue(C2), TOLERANCE);

    // This change should have an effective value of 0
    spreadsheet.setCellExpression(B2, "");
    assertEquals(8.0, spreadsheet.getCellValue(A1), TOLERANCE);
    assertEquals(8.0, spreadsheet.getCellValue(A2), TOLERANCE);
    assertEquals(0.0, spreadsheet.getCellValue(B2), TOLERANCE);
    assertEquals(16.0, spreadsheet.getCellValue(C2), TOLERANCE);

    spreadsheet.setCellExpression(B2, "A1 + A2");
    spreadsheet.setCellExpression(A2, "A1 + A1");
    assertEquals(8.0, spreadsheet.getCellValue(A1), TOLERANCE);
    assertEquals(16.0, spreadsheet.getCellValue(A2), TOLERANCE);
    assertEquals(24.0, spreadsheet.getCellValue(B2), TOLERANCE);
    assertEquals(48.0, spreadsheet.getCellValue(C2), TOLERANCE);

    spreadsheet.setCellExpression(A1, "1.0");
    assertEquals(1.0, spreadsheet.getCellValue(A1), TOLERANCE);
    assertEquals(2.0, spreadsheet.getCellValue(A2), TOLERANCE);
    assertEquals(3.0, spreadsheet.getCellValue(B2), TOLERANCE);
    assertEquals(6.0, spreadsheet.getCellValue(C2), TOLERANCE);
  }
}
