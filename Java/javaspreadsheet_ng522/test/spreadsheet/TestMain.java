package spreadsheet;

import static org.junit.Assert.assertThat;
import static spreadsheet.TokensMatcher.arrayMatchesTokens;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import org.junit.Test;

/**
 * Test the REPL implementation as part of the main class.
 *
 * <p>Rather than call `Main.main`, it uses `Main.interact`, which allows us to mock the standard
 * in
 * and out.
 *
 * <p>Similar to `TestParser`, the output is passed through the lexer to ignore any whitespace or
 * floating point differences.
 */
public class TestMain {

  @Test
  public void testPrintsPrompt() throws Exception {
    assertThat(interact(), arrayMatchesTokens(">"));
  }

  @Test
  public void testEvaluate() throws Exception {
    String[] output = interact("1 + 2", "2 * 4", "1 + 2 * 4");
    assertThat(output, arrayMatchesTokens("> 3", "> 8", "> 9", ">"));
  }

  @Test
  public void testEmptyCellIsZero() throws Exception {
    String[] output = interact("a1", "a1 = 5", "a2");
    assertThat(output, arrayMatchesTokens("> 0", "> > 0", ">"));
  }

  @Test
  public void testAssignement() throws Exception {
    String[] output = interact("a1 = 1 + 2", "a2 = a1 * 2", "a1 * a2");
    assertThat(output, arrayMatchesTokens("> > > 18", ">"));
  }

  @Test
  public void testReasssign() throws Exception {
    String[] output = interact("a1 = 1 + 2", "a2 = a1 * 2", "a1 * a2", "a1 = 5", "a1 * a2");
    assertThat(output, arrayMatchesTokens("> > > 18", "> > 30", ">"));
  }

  /**
   * Call Main.interact with a mock stdin and stdout.
   *
   * <p>Each element of `lines` will be fed into the input, and the output lines are collected and
   * returned.
   */
  private String[] interact(String... lines) throws Exception {
    StringBuilder inputBuffer = new StringBuilder();
    for (String line : lines) {
      inputBuffer.append(line);
      inputBuffer.append('\n');
    }

    ByteArrayOutputStream outputBuffer = new ByteArrayOutputStream();

    try (InputStream input =
        new ByteArrayInputStream(inputBuffer.toString().getBytes(StandardCharsets.UTF_8))) {
      try (PrintStream output = new PrintStream(outputBuffer, true, StandardCharsets.UTF_8)) {
        Main.interact(input, output);
      }
    }
    return outputBuffer.toString(StandardCharsets.UTF_8).split("\n");
  }
}
