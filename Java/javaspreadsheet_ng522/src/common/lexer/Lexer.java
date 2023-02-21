package common.lexer;

import common.api.CellLocation;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class Lexer {

  private static final Pattern CELL_LOCATION_REGEX = Pattern.compile("^[a-zA-Z]+[0-9]+$");
  private static final Pattern NUMBER_REGEX =
      Pattern.compile("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?");

  private final PushbackReader reader;

  public Lexer(String input) {
    reader = new PushbackReader(new StringReader(input), 1);
  }

  /**
   * Tokenize an entire string into a list of tokens.
   */
  public static List<Token> tokenize(String input) throws InvalidTokenException {
    List<Token> result = new ArrayList<>();
    Lexer lexer = new Lexer(input);
    while (true) {
      Token token = lexer.nextToken();
      if (token != null) {
        result.add(token);
      } else {
        break;
      }
    }
    return result;
  }

  /**
   * Get the next token from the lexer.
   *
   * <p>Returns null when there are no more tokens left.
   */
  public Token nextToken() throws InvalidTokenException {
    int c;
    do {
      c = readChar();
    } while (Character.isWhitespace(c));

    switch (c) {
      case -1:
        return null;
      case '+':
        return new Token(Token.Kind.PLUS);
      case '-':
        return new Token(Token.Kind.MINUS);
      case '*':
        return new Token(Token.Kind.STAR);
      case '/':
        return new Token(Token.Kind.SLASH);
      case '^':
        return new Token(Token.Kind.CARET);
      case '(':
        return new Token(Token.Kind.LPARENTHESIS);
      case ')':
        return new Token(Token.Kind.RPARENTHESIS);
      case '<':
        return new Token(Token.Kind.LANGLE);
      case '>':
        return new Token(Token.Kind.RANGLE);
      case '=':
        return new Token(Token.Kind.EQUALS);

      default:
        if (Character.isLetter(c)) {
          return readCellLocation((char) c);
        } else if (Character.isDigit(c)) {
          return readNumber((char) c);
        } else {
          throw new InvalidTokenException(Character.toString((char) c));
        }
    }
  }

  private Token readCellLocation(char initial) throws InvalidTokenException {
    String word = readWord(initial);
    if (!CELL_LOCATION_REGEX.matcher(word).matches()) {
      throw new InvalidTokenException(word);
    }
    return new Token(new CellLocation(word));
  }

  private Token readNumber(char initial) throws InvalidTokenException {
    String word = readWord(initial);
    try {
      return new Token(Double.parseDouble(word));
    } catch (NumberFormatException e) {
      throw new InvalidTokenException(word);
    }
  }

  private String readWord(char initial) {
    StringBuffer word = new StringBuffer();
    word.append(initial);

    while (true) {
      int c = readChar();
      if (Character.isLetterOrDigit(c) || c == '.') {
        word.append((char) c);
      } else {
        if (c != -1) {
          try {
            reader.unread((char) c);
          } catch (IOException e) {
            throw new RuntimeException();
          }
        }
        break;
      }
    }
    return word.toString();
  }

  private int readChar() {
    try {
      return reader.read();
    } catch (IOException e) {
      throw new IllegalStateException("reading from string should always work!");
    }
  }
}
