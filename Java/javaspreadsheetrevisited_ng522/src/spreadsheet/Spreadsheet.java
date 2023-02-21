package spreadsheet;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import common.api.Expression;
import common.lexer.InvalidTokenException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class Spreadsheet implements BasicSpreadsheet {
  //
  // start replacing
  //

  private final CycleDetector cycleDet = new CycleDetector(this);

  private final Map<CellLocation, Cell> cellMap = new HashMap<>();

  /**
   * Construct an empty spreadsheet.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  Spreadsheet() {
  }

  /**
   * Parse and evaluate an expression, using the spreadsheet as a context.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  public double evaluateExpression(String expression)
      throws InvalidSyntaxException {

    Expression exp = null;

    try {
      exp = Parser.parse(expression);
    } catch (InvalidTokenException e) {
      System.out.println("Idc about this");
    }

    assert exp != null;
    return exp.evaluate(this);
  }

  /**
   * Assign an expression to a cell.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  public void setCellExpression(CellLocation location, String input)
      throws InvalidSyntaxException {

    String prevExp = "";

    if (!cellMap.containsKey(location)) {
      createCell(location);
    } else {
      prevExp = cellMap.get(location).getExpression();
    }

    cellMap.get(location).setExpression(input);

    if (cycleDet.hasCycleFrom(location)) {
      setCellExpression(location, prevExp);
    } else {
      recalculate(location);
    }
  }

  private void createCell(CellLocation location) {
    cellMap.put(location, new Cell(this, location));
  }

  @Override
  public double getCellValue(CellLocation location) {
    if (cellMap.containsKey(location)) {
      return cellMap.get(location).getValue();
    } else {
      return 0;
    }
  }

  //
  // end replacing
  //

  @Override
  public String getCellExpression(CellLocation location) {
    if (cellMap.containsKey(location)) {
      return cellMap.get(location).getExpression();
    } else {
      return "";
    }
  }

  @Override
  public String getCellDisplay(CellLocation location) {
    if (cellMap.containsKey(location)) {
      return cellMap.get(location).toString();
    } else {
      return "";
    }
  }

  @Override
  public void addDependency(CellLocation dependent, CellLocation dependency) {
    if (!cellMap.containsKey(dependency)) {
      createCell(dependency);
    }
    cellMap.get(dependency).addDependent(dependent);
  }

  @Override
  public void removeDependency(CellLocation dependent, CellLocation dependency) {
    cellMap.get(dependency).removeDependent(dependent);
  }

  @Override
  public void recalculate(CellLocation location) {
    cellMap.get(location).recalculate();
  }

  @Override
  public void findCellReferences(CellLocation subject, Set<CellLocation> target) {
    cellMap.get(subject).findCellReferences(target);
  }
}
