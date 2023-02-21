package common.lexer;

public class InvalidTokenException extends Exception {

  public final String token;

  public InvalidTokenException(String token) {
    super(String.format("Invalid token \"%s\"", escapeString(token)));
    this.token = token;
  }

  private static String escapeString(String in) {
    StringBuilder out = new StringBuilder();
    for (int i = 0; i < in.length(); i++) {
      escapeChar(out, in.charAt(i));
    }
    return out.toString();
  }

  private static void escapeChar(StringBuilder out, char in) {
    // A few special cases, either non-displayable characters which have a short
    // escape sequence (\n,\r,\t), or displayable characters that have a special
    // meaning inside quotes (\, ").
    switch (in) {
      case '\n':
        out.append("\\n");
        return;
      case '\r':
        out.append("\\r");
        return;
      case '\t':
        out.append("\\t");
        return;
      case '\\':
        out.append("\\\\");
        return;
      case '"':
        out.append("\\\"");
        return;

      default:
    }

    // This is the displayable range of ASCII characters, we don't have to
    // escape those.
    if (in >= 0x20 && in < 0x7e) {
      out.append(in);
      return;
    }

    // Escape everything else by printing its code point in the \ uXXXX format.
    out.append(String.format("\\u%04d", (int) in));
  }
}
