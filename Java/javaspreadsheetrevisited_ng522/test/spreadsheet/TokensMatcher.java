package spreadsheet;

import static org.hamcrest.Matchers.arrayContaining;

import common.lexer.InvalidTokenException;
import common.lexer.Lexer;
import common.lexer.Token;
import java.util.ArrayList;
import java.util.List;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

/**
 * Compare a String against a list of expected tokens.
 *
 * <p>NUMBER tokens are compared approximately, allowing for small floating point differences.
 */
class TokensMatcher extends TypeSafeMatcher<String> {

  private static final double TOLERANCE = 0.0001;
  private final List<Token> expected;

  private TokensMatcher(List<Token> expected) {
    this.expected = expected;
  }

  public static TokensMatcher matchesTokens(List<Token> expected) {
    return new TokensMatcher(expected);
  }

  public static TokensMatcher matchesTokens(String input) {
    try {
      return matchesTokens(Lexer.tokenize(input));
    } catch (InvalidTokenException e) {
      throw new IllegalArgumentException(e);
    }
  }

  public static Matcher<String[]> arrayMatchesTokens(String... inputs) {
    List<Matcher<? super String>> matchers = new ArrayList<>();
    for (String input : inputs) {
      matchers.add(matchesTokens(input));
    }
    return arrayContaining(matchers);
  }

  @Override
  protected boolean matchesSafely(String actual) {
    List<Token> actualTokens;

    try {
      actualTokens = Lexer.tokenize(actual);
    } catch (InvalidTokenException e) {
      return false;
    }

    if (actualTokens.size() != expected.size()) {
      return false;
    }

    for (int i = 0; i < actualTokens.size(); i++) {
      Token left = actualTokens.get(i);
      Token right = expected.get(i);

      if (left.kind == Token.Kind.NUMBER && right.kind == Token.Kind.NUMBER) {
        if (Math.abs(right.numberValue - left.numberValue) > TOLERANCE) {
          return false;
        }
      } else {
        if (!left.equals(right)) {
          return false;
        }
      }
    }

    return true;
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(expected.toString());
  }
}
