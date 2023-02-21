package rectangles;

import java.util.Optional;

public class Rectangle {

  private final Point topLeft;
  private final Point bottomRight;

  public Rectangle(Point p1, Point p2) {
    topLeft = new Point(Math.min(p1.getX(), p2.getX()), Math.min(p1.getY(), p2.getY()));
    bottomRight = new Point(Math.max(p1.getX(), p2.getX()), Math.max(p1.getY(), p2.getY()));
  }

  public Rectangle(Point corner, int width, int height) {
    this(corner, corner.add(new Point(width, height)));
  }

  public Rectangle(int width, int height) {
    this(new Point(), new Point(width, height));
  }

  public int getWidth() {
    return bottomRight.getX() - topLeft.getX();
  }

  public int getHeight() {
    return bottomRight.getY() - topLeft.getY();
  }

  public Rectangle setWidth(int newWidth) {
    return new Rectangle(topLeft, newWidth, getHeight());
  }

  public Rectangle setHeight(int newHeight) {
    return new Rectangle(topLeft, getWidth(), newHeight);
  }

  public Point getTopLeft() {
    return topLeft;
  }

  public Point getTopRight() {
    return topLeft.add(new Point(getWidth()));
  }

  public Point getBottomLeft() {
    return topLeft.add(new Point(0, getHeight()));
  }

  public Point getBottomRight() {
    return bottomRight;
  }

  public int area() {
    return getHeight() * getWidth();
  }

  public boolean intersects(Rectangle other) {
    if (bottomRight.isLeftOf(other.topLeft)) {
      return false;
    }
    if (topLeft.isRightOf(other.bottomRight)) {
      return false;
    }
    if (bottomRight.isAbove(other.topLeft)) {
      return false;
    }
    if (topLeft.isBelow(other.bottomRight)) {
      return false;
    }
    return true;
  }

  public Rectangle translate(Point vector) {
    return new Rectangle(topLeft.add(vector), bottomRight.add(vector));
  }

  public Rectangle scale(int factor) {
    return new Rectangle(topLeft, getWidth() * factor, getHeight() * factor);
  }

  public Optional<Rectangle> intersection(Rectangle other) {
    if (!intersects(other)) {
      return Optional.empty();
    }

    return Optional.of(new Rectangle(
        new Point(Math.max(topLeft.getX(), other.topLeft.getX()),
            Math.max(topLeft.getY(), other.topLeft.getY())),
        new Point(Math.min(bottomRight.getX(), other.bottomRight.getX()),
            Math.min(bottomRight.getY(), other.bottomRight.getY()))));
  }

  public static Optional<Rectangle> intersection(Optional<Rectangle> r1, Optional<Rectangle> r2) {
    if (r1.isEmpty() || r2.isEmpty()) {
      return Optional.empty();
    }
    return r1.get().intersection(r2.get());
  }
}
