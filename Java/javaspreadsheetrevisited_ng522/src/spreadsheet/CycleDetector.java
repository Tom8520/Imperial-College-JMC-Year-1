package spreadsheet;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import java.util.HashSet;
import java.util.Set;

/**
 * Detects dependency cycles.
 */
public class CycleDetector {

  private final BasicSpreadsheet spreadsheet;

  /**
   * Constructs a new cycle detector.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param spreadsheet The parent spreadsheet, used for resolving cell locations.
   */
  CycleDetector(BasicSpreadsheet spreadsheet) {
    this.spreadsheet = spreadsheet;
  }

  /**
   * Checks for a cycle in the spreadsheet, starting at a particular cell.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param start The cell location where cycle detection should start.
   * @return Whether a cycle was detected in the dependency graph starting at the given cell.
   */
  public boolean hasCycleFrom(CellLocation start) {
    Set<CellLocation> visited = new HashSet<>();
    visited.add(start);

    return dfs(start, visited);
  }

  private boolean dfs(CellLocation start, Set<CellLocation> visited) {
    Set<CellLocation> refs = new HashSet<>();
    spreadsheet.findCellReferences(start, refs);
    for (CellLocation location : refs) {
      if (visited.contains(location)) {
        return true;
      }
      visited.add(location);
      if (dfs(location, visited)) {
        return true;
      }
      visited.remove(location);
    }
    return false;
  }
}
