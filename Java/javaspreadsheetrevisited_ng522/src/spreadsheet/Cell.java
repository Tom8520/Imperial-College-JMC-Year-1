package spreadsheet;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import common.api.Expression;
import common.lexer.InvalidTokenException;
import java.util.HashSet;
import java.util.Set;

/**
 * A single cell in a spreadsheet, tracking the expression, value, and other parts of cell state.
 */
public class Cell {

  private final BasicSpreadsheet parent;
  private final CellLocation location;
  private double value = 0;
  private Expression exp = null;

  private final Set<CellLocation> dependents = new HashSet<>();

  private boolean error = false;

  /**
   * Constructs a new cell.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param spreadsheet The parent spreadsheet,
   * @param location    The location of this cell in the spreadsheet.
   */
  Cell(BasicSpreadsheet spreadsheet, CellLocation location) {
    parent = spreadsheet;
    this.location = location;
  }

  /**
   * Gets the cell's last calculated value.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @return the cell's value.
   */
  public double getValue() {
    return value;
  }

  /**
   * Gets the cell's last stored expression, in string form.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @return a string that parses to an equivalent expression to that last stored in the cell; if no
   * expression is stored, we return the empty string.
   */
  public String getExpression() {
    if (exp == null) {
      return "";
    }
    return exp.toString();
  }

  /**
   * Sets the cell's expression from a string.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param input The string representing the new cell expression.
   * @throws InvalidSyntaxException if the string cannot be parsed.
   */
  public void setExpression(String input) throws InvalidSyntaxException {
    error = false;
    try {
      if (exp != null) {
        Set<CellLocation> deps = new HashSet<>();
        exp.findCellReferences(deps);

        deps.forEach((dep) -> {
          parent.removeDependency(location, dep);
        });
      }

      exp = Parser.parse(input);

      if (exp != null) {
        Set<CellLocation> deps = new HashSet<>();
        exp.findCellReferences(deps);

        deps.forEach((dep) -> {
          parent.addDependency(location, dep);
        });
      }
    } catch (InvalidTokenException e) {
      error = true;
    }
  }

  /**
   * @return a string representing the value, if any, of this cell.
   */
  @Override
  public String toString() {
    if (error) {
      return "#ERROR";
    }
    if (exp == null) {
      return "";
    } else {
      return Double.toString(value);
    }
  }

  /**
   * Adds the given location to this cell's dependents.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param location the location to add.
   */
  public void addDependent(CellLocation location) {
    dependents.add(location);
  }

  /**
   * Adds the given location to this cell's dependents.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param location the location to add.
   */
  public void removeDependent(CellLocation location) {
    dependents.remove(location);
  }

  /**
   * Adds this cell's expression's references to a set.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   *
   * @param target The set that will receive the dependencies for this
   */
  public void findCellReferences(Set<CellLocation> target) {
    if (exp != null) {
      exp.findCellReferences(target);
    }
  }

  /**
   * Recalculates this cell's value based on its expression.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  public void recalculate() {
    try {
      if (exp == null) {
        value = 0;
      } else {
        value = exp.evaluate(parent);
      }
      dependents.forEach(parent::recalculate);
    } catch (Exception e) {
      error = true;
    }
  }
}
