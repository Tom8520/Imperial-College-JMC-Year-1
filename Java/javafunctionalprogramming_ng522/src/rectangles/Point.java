package rectangles;

public class Point {

  private final int x;
  private final int y;

  public Point(int x, int y) {
    this.x = x;
    this.y = y;

    if (x < 0 || y < 0) {
      throw new NumberFormatException("Coordinates must be positive");
    }
  }

  public Point() {
    this(0, 0);
  }

  public Point(int x) {
    this(x, 0);
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public Point setX(int newX) {
    return new Point(newX, y);
  }

  public Point setY(int newY) {
    return new Point(x, newY);
  }

  public boolean isLeftOf(Point other) {
    return x < other.x;
  }

  public boolean isRightOf(Point other) {
    return x > other.x;
  }

  public boolean isAbove(Point other) {
    return y < other.y;
  }

  public boolean isBelow(Point other) {
    return y > other.y;
  }

  public Point add(Point vector) {
    return new Point(x + vector.x, y + vector.y);
  }
}
