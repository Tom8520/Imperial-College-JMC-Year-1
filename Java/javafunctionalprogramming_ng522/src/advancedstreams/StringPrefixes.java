package advancedstreams;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StringPrefixes {

  /**
   * Return the number of strings in the "strings" stream that start with the "prefix" string.
   */
  public static int countStringsStartingWithPrefix(Stream<String> strings, String prefix) {
    return (int) strings.filter(x -> x.startsWith(prefix)).count();
  }

  /**
   * If the "strings" stream contains a string that starts with "prefix", return this string but
   * with the prefixed part emphasised by enclosing it with asterisks. For example, if the prefix is
   * "foo" and the stream starts with "fooobar" then "*foo*bar" should be returned. If no string in
   * the stream starts with the given prefix, "N/A" should be returned.
   */
  public static String emphasiseFirstStringStartingWithPrefix(
      Stream<String> strings, String prefix) {
    return strings.filter(s -> s.startsWith(prefix))
        .map(s -> "*" + prefix + "*" + s.substring(prefix.length())).findFirst().orElse("N/A");
  }

  /**
   * Return a list containing the distinct strings in "strings" that start with prefix "prefix". The
   * resulting strings should be ordered according to their first occurrence in the input stream.
   */
  public static List<String> distinctStringsStartingWithPrefix(
      Stream<String> strings, String prefix) {
    return strings.filter(s -> s.startsWith(prefix)).distinct().collect(Collectors.toList());
  }
}
