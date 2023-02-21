package spreadsheet;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static spreadsheet.TokensMatcher.matchesTokens;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.junit.Test;

public class TestCell {

  private static final CellLocation A1 = new CellLocation("A1");
  private static final CellLocation A2 = new CellLocation("A2");
  private static final CellLocation B1 = new CellLocation("B1");
  private static final CellLocation B2 = new CellLocation("B2");

  /*
   * PART 2
   */

  /**
   * The empty cell should have a blank expression and the value 0.
   */
  @Test
  public void testInitialCell() {
    Cell cell = new Cell(new Spreadsheet(), A1);
    assertEquals("", cell.getExpression());
    assertEquals(0.0, cell.getValue(), 0.0);
  }

  /**
   * The empty expression should evaluate to 0, render as "", and be settable by passing in the
   * expression "".
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs
   */
  @Test
  public void testEmptyExpression() throws InvalidSyntaxException {
    Cell c = new Cell(new Spreadsheet(), A1);
    assertEquals("", c.getExpression());
    assertEquals(0.0, c.getValue(), 0.0);

    // this is just to make sure re-setting the empty expression clears the value etc.
    c.setExpression("42");
    c.recalculate();

    // now try setting the empty expression directly
    c.setExpression("");
    c.recalculate();
    assertEquals("", c.getExpression());
    assertEquals(0.0, c.getValue(), 0.0);
  }

  /**
   * The value shouldn't change if we insert a new expression, only when we recalculate it.
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs
   */
  @Test
  public void testValueLaziness() throws InvalidSyntaxException {
    Cell c = new Cell(new Spreadsheet(), A1);
    // should not get value immediately
    c.setExpression("5+5");
    assertEquals(0.0, c.getValue(), 0.0);
    // should now take value
    c.recalculate();
    assertEquals(10.0, c.getValue(), 0.0);
    // should retain value
    c.setExpression("1000");
    assertEquals(10.0, c.getValue(), 0.0);
  }

  /**
   * Tests that getExpression works.
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs.
   */
  @Test
  public void testGetExpression() throws InvalidSyntaxException {
    Spreadsheet s = new Spreadsheet();
    Cell c = new Cell(s, A1);

    // initial expression should be empty
    assertEquals("", c.getExpression());

    CellLocation loc = new CellLocation("a5");
    s.setCellExpression(loc, "5");
    c.setExpression("a5 * 6");
    c.recalculate();
    assertThat(c.getExpression(), matchesTokens("( a5 * 6 )"));
    assertEquals(30.0, c.getValue(), 0.0);
  }

  /**
   * Tests how setExpression interacts with the empty string.
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs.
   */
  @Test
  public void testSetExpressionEmpty() throws InvalidSyntaxException {
    Spreadsheet s = new Spreadsheet();
    Cell c = new Cell(s, A1);

    // setting expression empty while it's already empty should work
    c.setExpression("");
    c.recalculate();
    assertEquals("", c.getExpression());
    assertEquals("", c.toString());
    assertEquals(0.0, c.getValue(), 0.0);

    c.setExpression("20 + 22");
    c.recalculate();
    assertThat(c.getExpression(), matchesTokens("( 20 + 22 )"));
    assertThat(c.toString(), matchesTokens("42"));
    assertEquals(42.0, c.getValue(), 1.0);

    // clearing now should work
    c.setExpression("");
    c.recalculate();
    assertEquals("", c.getExpression());
    assertEquals("", c.toString());
    assertEquals(0.0, c.getValue(), 0.0);
  }

  /**
   * Tests that the cell's tracking of dependents seems to work properly with regards to
   * recalculations.
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs.
   */
  @Test
  public void testDependentRecalculate() throws InvalidSyntaxException {
    FakeSpreadsheet s = new FakeSpreadsheet();
    Cell c = new Cell(s, A1);

    // No dependencies
    c.setExpression("10.0");
    c.recalculate();

    // Then add dependents
    c.addDependent(A2);
    c.addDependent(B1);
    c.setExpression("20.0");
    c.recalculate();
    assertEquals(0, s.recalculationCount(A1));
    assertEquals(1, s.recalculationCount(A2));
    assertEquals(1, s.recalculationCount(B1));
    assertEquals(0, s.recalculationCount(B2));

    // Now switch a few dependents
    c.removeDependent(A2);
    c.addDependent(B2);
    c.setExpression("30.0");
    c.recalculate();
    assertEquals(0, s.recalculationCount(A1));
    assertEquals(1, s.recalculationCount(A2));
    assertEquals(2, s.recalculationCount(B1));
    assertEquals(1, s.recalculationCount(B2));

    // And check empty expressions
    c.removeDependent(B1);
    c.setExpression("");
    c.recalculate();
    assertEquals(0, s.recalculationCount(A1));
    assertEquals(1, s.recalculationCount(A2));
    assertEquals(2, s.recalculationCount(B1));
    assertEquals(2, s.recalculationCount(B2));
  }

  /**
   * Tests whether cells accurately report their references.
   *
   * @throws InvalidSyntaxException if a spurious parse error occurs.
   */
  @Test
  public void testFindCellReferences() throws InvalidSyntaxException {
    Spreadsheet s = new Spreadsheet();
    Cell c = new Cell(s, A1);

    // initial expression should have no references
    assertTrue(dependencies(c).isEmpty());

    c.setExpression("a2 + b2");
    assertThat(dependencies(c), containsInAnyOrder("a2", "b2"));

    c.setExpression("c1 + c1 * c1");
    assertThat(dependencies(c), containsInAnyOrder("c1"));

    // cleared expression should have no references
    c.setExpression("");
    assertTrue(dependencies(c).isEmpty());
  }

  private Set<String> dependencies(Cell c) {
    Set<CellLocation> cells = new HashSet<>();
    c.findCellReferences(cells);

    Set<String> result = new HashSet<>();
    for (CellLocation cell : cells) {
      result.add(cell.toString());
    }
    return result;
  }

  /**
   * Fake spreadsheet used to check whether dependency resolution is working properly.
   */
  private static class FakeSpreadsheet implements BasicSpreadsheet {

    private final Map<CellLocation, Integer> recalculations = new HashMap<>();

    /**
     * @param location the location to check.
     * @return the number of times location was asked to recalculate.
     */
    public int recalculationCount(CellLocation location) {
      return this.recalculations.getOrDefault(location, 0);
    }

    @Override
    public String getCellDisplay(CellLocation location) {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getCellExpression(CellLocation location) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void setCellExpression(CellLocation location, String expression) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void addDependency(CellLocation dependent, CellLocation dependency) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void removeDependency(CellLocation dependent, CellLocation dependency) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void findCellReferences(CellLocation subject, Set<CellLocation> target) {
      throw new UnsupportedOperationException();
    }

    /**
     * Pretends to recalculate, but actually just makes a note that a particular cell location was
     * asked to recalculate.
     *
     * @param location The location to recalculate.
     */
    @Override
    public void recalculate(CellLocation location) {
      this.recalculations.compute(location, (_l, current) -> current == null ? 1 : current + 1);
    }

    @Override
    public double getCellValue(CellLocation location) {
      throw new UnsupportedOperationException();
    }
  }
}
