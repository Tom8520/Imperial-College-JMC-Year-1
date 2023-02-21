package spreadsheet;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.junit.Test;

public class TestCycleDetector {

  private static final CellLocation A1 = new CellLocation("a1");
  private static final CellLocation A2 = new CellLocation("a2");
  private static final CellLocation B1 = new CellLocation("b1");
  private static final CellLocation B2 = new CellLocation("b2");

  /**
   * Tests that the cycle detector detects no cycles in an empty sheet.
   */
  @Test
  public void testEmpty() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    assertFalse(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector detects a direct cycle.
   */
  @Test
  public void testDirect() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A1});

    assertTrue(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector detects a direct cycle at the end of a chain.
   */
  @Test
  public void testChainedDirect() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2});
    t.dependencyEdges.put(A2, new CellLocation[]{B2});
    t.dependencyEdges.put(B2, new CellLocation[]{B1});
    t.dependencyEdges.put(B1, new CellLocation[]{B1});

    assertTrue(c.hasCycleFrom(A1));
    assertTrue(c.hasCycleFrom(A2));
    assertTrue(c.hasCycleFrom(B1));
    assertTrue(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector detects a small indirect cycle.
   */
  @Test
  public void testSmallIndirect() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2});
    t.dependencyEdges.put(A2, new CellLocation[]{A1});

    assertTrue(c.hasCycleFrom(A1));
    assertTrue(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector detects a large, very indirect cycle.
   */
  @Test
  public void testLargeIndirect() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2});
    t.dependencyEdges.put(A2, new CellLocation[]{B2});
    t.dependencyEdges.put(B2, new CellLocation[]{B1});
    t.dependencyEdges.put(B1, new CellLocation[]{A1});

    assertTrue(c.hasCycleFrom(A1));
    assertTrue(c.hasCycleFrom(A2));
    assertTrue(c.hasCycleFrom(B1));
    assertTrue(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector ignores a diamond dependency pattern.
   */
  @Test
  public void testDiamond() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2, B1});
    t.dependencyEdges.put(A2, new CellLocation[]{B2});
    t.dependencyEdges.put(B1, new CellLocation[]{B2});

    assertFalse(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector ignores a triangle dependency pattern.
   */
  @Test
  public void testTriangle() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2, B2});
    t.dependencyEdges.put(A2, new CellLocation[]{B2});

    assertFalse(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector ignores a hub dependency pattern.
   */
  @Test
  public void testHub() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{B2});
    t.dependencyEdges.put(A2, new CellLocation[]{B2});
    t.dependencyEdges.put(B1, new CellLocation[]{B2});

    assertFalse(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * Tests that the cycle detector ignores an inverted hub dependency pattern.
   */
  @Test
  public void testInvertedHub() {
    FakeSpreadsheet t = new FakeSpreadsheet();
    CycleDetector c = new CycleDetector(t);

    t.dependencyEdges.put(A1, new CellLocation[]{A2, B1, B2});

    assertFalse(c.hasCycleFrom(A1));
    assertFalse(c.hasCycleFrom(A2));
    assertFalse(c.hasCycleFrom(B1));
    assertFalse(c.hasCycleFrom(B2));
  }

  /**
   * A fake spreadsheet that only implements the methods needed for cycle detection.
   */
  private static class FakeSpreadsheet implements BasicSpreadsheet {

    /**
     * The dependency graph exposed to the cycle detector by the fake spreadsheet.
     */
    public Map<CellLocation, CellLocation[]> dependencyEdges = new HashMap<>();

    @Override
    public double getCellValue(CellLocation location) {
      throw new UnsupportedOperationException();
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
      target.addAll(
          Arrays.asList(this.dependencyEdges.getOrDefault(subject, new CellLocation[]{})));
    }

    @Override
    public void recalculate(CellLocation location) {
      throw new UnsupportedOperationException();
    }
  }
}
