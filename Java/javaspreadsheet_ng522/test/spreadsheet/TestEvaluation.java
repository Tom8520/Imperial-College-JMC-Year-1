package spreadsheet;

import static org.junit.Assert.assertThat;
import static spreadsheet.EvaluationMatcher.evaluatesTo;

import org.junit.Test;

public class TestEvaluation {

  @Test
  public void testNumber() {
    assertThat("5", evaluatesTo(5));
    assertThat("10", evaluatesTo(10));
    assertThat("5.678", evaluatesTo(5.678));
  }

  @Test
  public void testCellLocation() {
    assertThat("a1", evaluatesTo(0));
    assertThat("a1", evaluatesTo(5).with("a1", 5));
    assertThat("b1", evaluatesTo(5).with("b1", 5));
    assertThat("abc123", evaluatesTo(-5).with("abc123", -5));
    assertThat("a1", evaluatesTo(0));
  }

  @Test
  public void testBinaryOperator() {
    assertThat("2 + 4", evaluatesTo(6));
    assertThat("2 * 4", evaluatesTo(8));
    assertThat("2 - 4", evaluatesTo(-2));
    assertThat("2 / 4", evaluatesTo(0.5));
    assertThat("2 ^ 4", evaluatesTo(16));

    assertThat("a1 + 3", evaluatesTo(5).with("a1", 2));
    assertThat("a1 + b1", evaluatesTo(6).with("a1", 2).with("b1", 4));
  }
}
