package advancedstreams;

import java.util.NoSuchElementException;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class CubeSupplier implements Supplier<Integer> {

  private int counter = 0;
  private static final int MAX_CUBE = (int) Math.cbrt(Integer.MAX_VALUE);

  public CubeSupplier() {

  }

  CubeSupplier(int start) {
    counter = start;
  }

  public Integer get() {
    counter++;
    if (counter > MAX_CUBE) {
      throw new NoSuchElementException();
    }
    return counter * counter * counter;
  }

  public static Stream<Integer> cubeStream() {
    return Stream.generate(new CubeSupplier());
  }

  public static Stream<Integer> boundedCubeStream(int start, int end) {
    return Stream.generate(new CubeSupplier(start)).limit(end - start);
  }

  public static Stream<Integer> palindromicCubes(int start, int end) {
    return boundedCubeStream(start, end).filter(x -> IsPalindrome.isPalindrome(String.valueOf(x)));
  }
}
