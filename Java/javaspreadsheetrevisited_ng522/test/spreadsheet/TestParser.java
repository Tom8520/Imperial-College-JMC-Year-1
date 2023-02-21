package spreadsheet;

import static org.junit.Assert.assertThat;
import static spreadsheet.TokensMatcher.matchesTokens;

import org.junit.Test;

/*
 * This test checks the string representation of a parsed AST, by calling
 * `Parser.parse(...).toString()`. This lets us be independent of the AST
 * definitions.
 *
 * In order for precedence and associativity to be tested, the toString() output
 * should wrap all binary operator expressions with parentheses.
 *
 * To avoid being too sensitive to formatting issues, the test actually compares
 * the tokenized result of `toString()`. This removes any extraneous whitespace
 * and allows for approximate comparison of number tokens.
 */
public class TestParser {

  @Test
  public void testNumber() throws Exception {
    check("1", "1");
    check("5", "5");
    check("5.5", "5.5");
    check("5.500", "5.5");
    check("5.", "5.");
  }

  @Test
  public void testCellLocation() throws Exception {
    check("a0", "a0");
    check("a1", "a1");
    check("abc1", "abc1");
  }

  @Test
  public void testBinaryOperator() throws Exception {
    check("1 + 2", "(1 + 2)");
    check("1 * 2", "(1 * 2)");
    check("1 - 2", "(1 - 2)");
    check("1 / 2", "(1 / 2)");
    check("1 ^ 2", "(1 ^ 2)");
    check("a1 + 2", "(a1 + 2)");
    check("a1 * 2", "(a1 * 2)");
    check("a1 - 2", "(a1 - 2)");
    check("a1 / 2", "(a1 / 2)");
    check("a1 ^ 2", "(a1 ^ 2)");
    check("a1 + b2", "(a1 + b2)");
    check("a1 * b2", "(a1 * b2)");
    check("a1 - b2", "(a1 - b2)");
    check("a1 / b2", "(a1 / b2)");
    check("a1 ^ b2", "(a1 ^ b2)");
  }

  @Test
  public void testPrecedence() throws Exception {
    check("1 + 2 * 4", "(1 + (2 * 4))");
    check("1 * 2 + 4", "((1 * 2) + 4)");
    check("1 * 2 / 4", "((1 * 2) / 4)");
    check("1 * 2 ^ 4", "(1 * (2 ^ 4))");
    check("1 + 2 ^ 4", "(1 + (2 ^ 4))");
    check("1 * 2 + 3 ^ 4", "((1 * 2) + (3 ^ 4))");
    check("1 - 2 + 4", "((1 - 2) + 4)");
  }

  @Test
  public void testAssociativity() throws Exception {
    check("1 * 2 * 4", "((1 * 2) * 4)");
    check("1 / 2 / 4", "((1 / 2) / 4)");
    check("1 + 2 + 4", "((1 + 2) + 4)");
    check("1 - 2 - 4", "((1 - 2) - 4)");
    check("1 ^ 2 ^ 4", "(1 ^ (2 ^ 4))");
  }

  private void check(String input, String expected) throws Exception {
    assertThat(Parser.parse(input).toString(), matchesTokens(expected));
  }
}
