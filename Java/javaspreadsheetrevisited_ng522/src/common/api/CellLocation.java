package common.api;

/**
 * Represents a cell's location in the tabular.
 */
public final class CellLocation {

  private final String representation;

  /**
   * Create a cell location from its string representation.
   *
   * <p>The representation will be normalized to be lower-cased.
   */
  public CellLocation(String input) {
    if (input == null) {
      throw new NullPointerException("input cannot be null");
    }
    this.representation = input.toLowerCase().trim();
  }

  /**
   * Create a cell location from its integer row and column.
   *
   * <p>`row` and `column` are zero-indexed. For example (0, 0) corresponds to a1, (1, 1)
   * corresponds to b2, ...
   */
  public CellLocation(int row, int column) {
    assert column >= 0 && row >= 0;
    representation = getColumnName(column) + (row + 1);
  }

  public static String getColumnName(int column) {
    StringBuilder sb = new StringBuilder();
    do {
      sb.append((char) ('a' + column % 26));
      column = column / 26;
    } while (column > 0);
    return sb.toString();
  }

  public String toString() {
    return representation;
  }

  @Override
  public int hashCode() {
    return representation.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    CellLocation other = (CellLocation) obj;

    return representation.equals(other.representation);
  }
}
