package common.api;

import java.util.Set;
import spreadsheet.InvalidSyntaxException;

/**
 * Minimal interface for spreadsheets.
 */
public interface BasicSpreadsheet extends EvaluationContext {

  /**
   * @param location The cell location to query.
   * @return the display string of the cell at the given location.
   */
  String getCellDisplay(CellLocation location);

  /**
   * @param location The cell location to query.
   * @return the expression of the cell at the given location.
   */
  String getCellExpression(CellLocation location);

  /**
   * Sets the expression of a cell, possibly triggering updates on the cell and its dependents.
   *
   * @param location   The cell location to modify.
   * @param expression The new expression of the cell.
   * @throws InvalidSyntaxException if the expression has invalid syntax.
   */
  void setCellExpression(CellLocation location, String expression) throws InvalidSyntaxException;

  /**
   * Adds a dependency edge.
   *
   * @param dependent  The cell that is dependent on the other cell.
   * @param dependency The cell that is the dependency of the other cell.
   */
  void addDependency(CellLocation dependent, CellLocation dependency);

  /**
   * Removes a dependency edge.
   *
   * @param dependent  The cell that is dependent on the other cell.
   * @param dependency The cell that is the dependency of the other cell.
   */
  void removeDependency(CellLocation dependent, CellLocation dependency);

  /**
   * Retrieves the cells referenced by a given cell location.
   *
   * @param subject The subject of the query.
   * @param target  The set into which the referenced cells should be collected.
   */
  void findCellReferences(CellLocation subject, Set<CellLocation> target);

  /**
   * Instructs the cell at a given location to recalculate.
   *
   * @param location The location to recalculate.
   */
  void recalculate(CellLocation location);
}
