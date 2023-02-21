package spreadsheet;

import common.api.CellLocation;
import common.api.EvaluationContext;
import common.api.Expression;
import common.lexer.InvalidTokenException;
import java.util.HashMap;
import java.util.Map;

public class Spreadsheet implements EvaluationContext {

  private final Map<CellLocation, Double> latestValues = new HashMap<>();

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
      throws InvalidSyntaxException, InvalidTokenException {
    Expression exp = Parser.parse(expression);

    return exp.evaluate(this);
  }

  /**
   * Assign an expression to a cell.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  public void setCellExpression(CellLocation location, String input)
      throws InvalidSyntaxException, InvalidTokenException {
    latestValues.put(location, evaluateExpression(input));
  }

  @Override
  public double getCellValue(CellLocation location) {
    if (latestValues.containsKey(location)) {
      return latestValues.get(location);
    } else {
      return 0;
    }
  }
}
