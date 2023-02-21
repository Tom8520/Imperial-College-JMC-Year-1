package spreadsheet;

import common.api.CellLocation;
import common.api.EvaluationContext;
import common.api.Expression;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

class EvaluationMatcher extends TypeSafeMatcher<Object> implements EvaluationContext {

  private static final double TOLERANCE = 0.0001;
  private final Map<CellLocation, Double> cells = new HashMap<>();
  private final double expectedValue;

  private EvaluationMatcher(double value) {
    this.expectedValue = value;
  }

  public static EvaluationMatcher evaluatesTo(double value) {
    return new EvaluationMatcher(value);
  }

  public EvaluationMatcher with(CellLocation location, double value) {
    cells.put(location, value);
    return this;
  }

  public EvaluationMatcher with(String location, double value) {
    return with(new CellLocation(location), value);
  }

  @Override
  protected boolean matchesSafely(Object actual) {
    Expression expression;

    if (actual instanceof Expression) {
      expression = (Expression) actual;
    } else if (actual instanceof String) {
      try {
        expression = Parser.parse((String) actual);
      } catch (Exception e) {
        return false;
      }
    } else {
      return false;
    }

    double actualValue = expression.evaluate(this);
    return Math.abs(actualValue - expectedValue) <= TOLERANCE;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText("Should evaluate to ");
    description.appendValue(expectedValue);
  }

  @Override
  public void describeMismatchSafely(final Object actual, final Description description) {
    if (!(actual instanceof String) && !(actual instanceof Expression)) {
      description.appendValue(actual);
      description.appendText(" is neither a String nor an Expression.");
      return;
    }

    super.describeMismatchSafely(actual, description);
  }

  @Override
  public double getCellValue(CellLocation location) {
    Double value = cells.get(location);
    if (value == null) {
      return 0;
    } else {
      return value;
    }
  }
}
