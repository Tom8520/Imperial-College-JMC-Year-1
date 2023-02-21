import static java.util.stream.Collectors.toList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import advancedstreams.CubeSupplier;
import advancedstreams.StringPrefixes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Test;
import rectangles.ListAlgorithms;
import rectangles.Point;
import rectangles.Rectangle;
import rectangles.StreamAlgorithms;

public class TestSuite {

  // Un-comment when you are ready to test your Point class.

  //==================================================================================
  // Helper methods to compare points and lists of points.
  // Later in the course we will look at overriding the "equals" method of Object,
  // eliminating the need for these sorts of methods.
  //==================================================================================

  private static boolean equalPoints(Point p1, Point p2) {
    return p1.getX() == p2.getX() && p1.getY() == p2.getY();
  }

  private static boolean equalPointLists(List<Point> l1, List<Point> l2) {
    if (l1.size() != l2.size()) {
      return false;
    }
    for (int i = 0; i < l1.size(); i++) {
      if (!equalPoints(l1.get(i), l2.get(i))) {
        return false;
      }
    }
    return true;
  }

  //==========================
  // 1. Tests for Point class.
  //==========================

  @Test
  public void pointConstructor1() {
    assertTrue(equalPoints(new Point(0, 0), new Point()));
  }

  @Test
  public void pointConstructor2() {
    assertTrue(equalPoints(new Point(1, 0), new Point(1)));
  }

  @Test
  public void pointGetX() {
    assertEquals(new Point(42, 52).getX(), 42);
  }

  @Test
  public void pointGetY() {
    assertEquals(new Point(42, 52).getY(), 52);
  }

  @Test
  public void pointSetX() {
    assertTrue(equalPoints(new Point(42, 52).setX(12), new Point(12, 52)));
  }

  @Test
  public void pointSetY() {
    assertTrue(equalPoints(new Point(42, 52).setY(12), new Point(42, 12)));
  }

  @Test
  public void pointIsLeftOf() {
    assertTrue(new Point(10, 20).isLeftOf(new Point(20, 20)));
    assertFalse(new Point(10, 11).isLeftOf(new Point(10, 11)));
  }

  @Test
  public void pointIsRightOf() {
    assertTrue(new Point(10, 20).isRightOf(new Point(0, 20)));
    assertFalse(new Point(10, 11).isRightOf(new Point(12, 11)));
  }

  @Test
  public void pointIsAbove() {
    assertTrue(new Point(1, 1).isAbove(new Point(1, 2)));
    assertFalse(new Point(10, 11).isAbove(new Point(0, 0)));
  }

  @Test
  public void pointIsBelow() {
    assertTrue(new Point(1, 1).isBelow(new Point(1, 0)));
    assertFalse(new Point(10, 11).isBelow(new Point(11, 11)));
  }

  @Test
  public void pointAdd() {
    assertTrue(equalPoints(new Point(1, 1).add(new Point(10, 10)), new Point(11, 11)));
    assertTrue(equalPoints(new Point(10, 0).add(new Point(0, 10)),
        new Point(0, 10).add(new Point(10, 0))));
  }

  //==================================================================================
  // Helper methods to compare rectangles and lists of rectangles.
  // Later in the course we will look at overriding the "equals" method of Object,
  // eliminating the need for these sorts of methods.
  //==================================================================================

  private static boolean equalRectangles(Rectangle r1, Rectangle r2) {
    return equalPoints(r1.getTopLeft(), r2.getTopLeft()) &&
        equalPoints(r1.getTopRight(), r2.getTopRight()) &&
        equalPoints(r1.getBottomLeft(), r2.getBottomLeft()) &&
        equalPoints(r1.getBottomRight(), r2.getBottomRight()) &&
        r1.getWidth() == r2.getWidth() && r2.getHeight() == r2.getHeight();
  }

  private static boolean equalRectangleLists(List<Rectangle> l1, List<Rectangle> l2) {
    if (l1.size() != l2.size()) {
      return false;
    }
    for (int i = 0; i < l1.size(); i++) {
      if (!equalRectangles(l1.get(i), l2.get(i))) {
        return false;
      }
    }
    return true;
  }

  //==============================
  // 2. Tests for Rectangle class.
  //==============================

  @Test
  public void rectangleConstructor1() {
    assertTrue(equalRectangles(new Rectangle(new Point(1, 3), 4, 5),
        new Rectangle(new Point(1, 3), new Point(5, 8))));
    assertTrue(equalRectangles(new Rectangle(new Point(1, 3), 4, 5),
        new Rectangle(new Point(5, 8), new Point(1, 3))));
  }

  @Test
  public void rectangleConstructor2() {
    assertTrue(equalRectangles(new Rectangle(10, 20), new Rectangle(new Point(0, 0), 10, 20)));
    assertTrue(
        equalRectangles(new Rectangle(10, 20), new Rectangle(new Point(0, 0), new Point(10, 20))));
  }

  @Test
  public void rectangleConstructor3() {
    assertTrue(equalRectangles(
        new Rectangle(new Point(10, 1), new Point(0, 10)),
        new Rectangle(new Point(0, 1), 10, 9)));
  }

  @Test
  public void rectangleGetWidth() {
    assertEquals(new Rectangle(new Point(1, 1), new Point(11, 11)).getWidth(), 10);
    assertEquals(new Rectangle(new Point(1, 1), 20, 30).getWidth(), 20);
  }

  @Test
  public void rectangleGetHeight() {
    assertEquals(new Rectangle(new Point(1, 0), new Point(11, 11)).getHeight(), 11);
    assertEquals(new Rectangle(new Point(7, 6), 20, 30).getHeight(), 30);
  }

  @Test
  public void rectangleSetWidth() {
    assertTrue(equalRectangles(new Rectangle(new Point(1, 2), 3, 4).setWidth(5),
        new Rectangle(new Point(1, 2), 5, 4)));
    assertTrue(equalRectangles(new Rectangle(new Point(1, 2), new Point(6, 7)).setWidth(10),
        new Rectangle(new Point(1, 2), new Point(11, 7))));
  }

  @Test
  public void rectangleSetHeight() {
    assertTrue(equalRectangles(new Rectangle(new Point(1, 2), 3, 4).setHeight(5),
        new Rectangle(new Point(1, 2), 3, 5)));
    assertTrue(equalRectangles(new Rectangle(new Point(1, 2), new Point(6, 7)).setHeight(10),
        new Rectangle(new Point(1, 2), new Point(6, 12))));
  }

  @Test
  public void rectangleGetTopLeft() {
    assertTrue(equalPoints(new Point(1, 2), new Rectangle(new Point(1, 2), 3, 4).getTopLeft()));
  }

  @Test
  public void rectangleGetTopRight() {
    assertTrue(equalPoints(new Point(4, 2), new Rectangle(new Point(1, 2), 3, 4).getTopRight()));
  }

  @Test
  public void rectangleGetBottomLeft() {
    assertTrue(equalPoints(new Point(1, 6), new Rectangle(new Point(1, 2), 3, 4).getBottomLeft()));
  }

  @Test
  public void rectangleGetBottomRight() {
    assertTrue(equalPoints(new Point(4, 6), new Rectangle(new Point(1, 2), 3, 4).getBottomRight()));
  }

  @Test
  public void rectangleArea() {
    assertEquals(100, new Rectangle(10, 10).area());
    assertEquals(30, new Rectangle(new Point(), new Point(6, 5)).area());
  }

  @Test
  public void rectangleIntersect1() {
    Rectangle r1 = new Rectangle(new Point(0, 0), 10, 10);
    Rectangle r2 = new Rectangle(new Point(8, 0), 10, 10);
    Rectangle r3 = new Rectangle(new Point(0, 8), 10, 10);
    Rectangle r4 = new Rectangle(new Point(8, 8), 10, 10);

    Rectangle r5 = new Rectangle(new Point(0, 100), 5, 5);
    Rectangle r6 = new Rectangle(new Point(100, 100), 5, 5);

    assertTrue(r1.intersects(r2));
    assertTrue(r1.intersects(r3));
    assertTrue(r1.intersects(r4));
    assertTrue(r2.intersects(r3));
    assertTrue(r2.intersects(r4));
    assertTrue(r3.intersects(r4));
    assertTrue(r1.intersects(r1));
    assertTrue(r2.intersects(r2));
    assertTrue(r3.intersects(r3));
    assertTrue(r4.intersects(r4));

    assertFalse(r1.intersects(r5));
    assertFalse(r2.intersects(r5));
    assertFalse(r3.intersects(r5));
    assertFalse(r4.intersects(r5));

    assertFalse(r1.intersects(r6));
    assertFalse(r2.intersects(r6));
    assertFalse(r3.intersects(r6));
    assertFalse(r4.intersects(r6));

    assertFalse(r5.intersects(r6));

  }

  @Test
  public void rectangleIntersect2() {
    assertTrue(
        new Rectangle(new Point(0, 0), 3, 3).intersects(new Rectangle(new Point(3, 3), 10, 10)));
    assertTrue(
        new Rectangle(new Point(0, 0), 3, 3).intersects(new Rectangle(new Point(3, 0), 10, 10)));
  }

  @Test
  public void rectangleIntersect3() {
    assertFalse(
        new Rectangle(new Point(0, 0), 3, 3).intersects(new Rectangle(new Point(4, 4), 10, 10)));
  }

  //===============================================================================
  // Lists of Rectangles, useful for testing ListAlgorithms and StreamAlgorithms.
  //===============================================================================

  private static final List<Rectangle> INCREASING_ENCLOSING = Arrays.asList(
      new Rectangle(1, 1),
      new Rectangle(2, 2),
      new Rectangle(3, 3),
      new Rectangle(4, 4),
      new Rectangle(5, 5));

  private static final List<Rectangle> VARIOUS_RECTANGLES = Arrays.asList(
      new Rectangle(new Point(10, 10), 10, 10),
      new Rectangle(new Point(15, 15), 10, 10),
      new Rectangle(new Point(0, 0), 1000, 1000),
      new Rectangle(new Point(2000, 2000), 1, 1));

  private static final List<Rectangle> VARIOUS_RECTANGLES_TRANSLATED = Arrays.asList(
      new Rectangle(new Point(20, 110), 10, 10),
      new Rectangle(new Point(25, 115), 10, 10),
      new Rectangle(new Point(10, 100), 1000, 1000),
      new Rectangle(new Point(2010, 2100), 1, 1));

  private static final List<Rectangle> VARIOUS_RECTANGLES_SCALED = Arrays.asList(
      new Rectangle(new Point(10, 10), 100, 100),
      new Rectangle(new Point(15, 15), 100, 100),
      new Rectangle(new Point(0, 0), 10000, 10000),
      new Rectangle(new Point(2000, 2000), 10, 10));

  private static final List<Rectangle> VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED = Arrays.asList(
      new Rectangle(new Point(20, 110), 100, 100),
      new Rectangle(new Point(25, 115), 100, 100),
      new Rectangle(new Point(10, 100), 10000, 10000),
      new Rectangle(new Point(2010, 2100), 10, 10));

  //===================================
  // 3. Tests for ListAlgorithms class.
  //===================================

  @Test
  public void listTranslate() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_TRANSLATED,
        ListAlgorithms.translate(VARIOUS_RECTANGLES, new Point(10, 100))));
  }

  @Test
  public void listScale() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_SCALED, ListAlgorithms.scale(VARIOUS_RECTANGLES, 10)));
  }

  @Test
  public void listTranslateAndScale() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED, ListAlgorithms.scale(
            ListAlgorithms.translate(VARIOUS_RECTANGLES, new Point(10, 100)),
            10)));
  }

  @Test
  public void listGetBottomLeft() {
    List<Point> bottomLeft = ListAlgorithms.getBottomLeftPoints(VARIOUS_RECTANGLES);
    List<Point> expected = Arrays.asList(
        new Point(10, 20),
        new Point(15, 25),
        new Point(0, 1000),
        new Point(2000, 2001));
    assertTrue(equalPointLists(bottomLeft, expected));
  }

  @Test
  public void listGetAllIntersecting1() {
    assertTrue(equalRectangleLists(INCREASING_ENCLOSING,
        ListAlgorithms.getAllIntersecting(INCREASING_ENCLOSING, INCREASING_ENCLOSING.get(2))));
  }

  @Test
  public void listGetAllIntersecting2() {
    assertTrue(equalRectangleLists(new ArrayList<Rectangle>(),
        ListAlgorithms.getAllIntersecting(new ArrayList<>(), new Rectangle(0, 0))));
  }

  @Test
  public void listGetAllIntersecting3() {
    Rectangle candidate = new Rectangle(new Point(3, 3), 100, 100);
    List<Rectangle> expected = Arrays
        .asList(INCREASING_ENCLOSING.get(2), INCREASING_ENCLOSING.get(3),
            INCREASING_ENCLOSING.get(4));
    assertTrue(equalRectangleLists(expected,
        ListAlgorithms.getAllIntersecting(INCREASING_ENCLOSING, candidate)));
  }

  @Test
  public void listGetAllWithBiggerAreaThan1() {
    Rectangle candidate = new Rectangle(new Point(3, 3), 100, 100);
    List<Rectangle> expected = new ArrayList<>();
    assertTrue(equalRectangleLists(expected,
        ListAlgorithms.getAllWithBiggerAreaThan(INCREASING_ENCLOSING, candidate)));
  }

  @Test
  public void listGetAllWithBiggerAreaThan2() {
    Rectangle candidate = new Rectangle(new Point(100, 100), 1, 1);
    List<Rectangle> expected = Arrays
        .asList(INCREASING_ENCLOSING.get(1), INCREASING_ENCLOSING.get(2),
            INCREASING_ENCLOSING.get(3), INCREASING_ENCLOSING.get(4));
    assertTrue(equalRectangleLists(expected,
        ListAlgorithms.getAllWithBiggerAreaThan(INCREASING_ENCLOSING, candidate)));
  }

  @Test
  public void listFindLargestArea() {
    assertEquals(5 * 5, ListAlgorithms.findLargestArea(INCREASING_ENCLOSING));
    assertEquals(1000 * 1000, ListAlgorithms.findLargestArea(VARIOUS_RECTANGLES));
    assertEquals(1000 * 1000, ListAlgorithms.findLargestArea(VARIOUS_RECTANGLES_TRANSLATED));
    assertEquals(10000 * 10000, ListAlgorithms.findLargestArea(VARIOUS_RECTANGLES_SCALED));
    assertEquals(10000 * 10000,
        ListAlgorithms.findLargestArea(VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED));
    assertEquals(0, ListAlgorithms.findLargestArea(new ArrayList<>()));
  }

  @Test
  public void listFindMaxHeight() {
    assertEquals(5, ListAlgorithms.findMaxHeight(INCREASING_ENCLOSING));
    assertEquals(1000, ListAlgorithms.findMaxHeight(VARIOUS_RECTANGLES));
    assertEquals(1000, ListAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_TRANSLATED));
    assertEquals(10000, ListAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_SCALED));
    assertEquals(10000, ListAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED));
  }

  @Test
  public void listGetSumOfAreas() {
    assertEquals(1 * 1 + 2 * 2 + 3 * 3 + 4 * 4 + 5 * 5,
        ListAlgorithms.getSumOfAreas(INCREASING_ENCLOSING));
  }

  @Test
  public void listGetSumOfAreasOfAllIntersecting() {
    assertEquals(0, ListAlgorithms.getSumOfAreasOfAllIntersecting(INCREASING_ENCLOSING,
        new Rectangle(new Point(100, 100), 10, 10)));
    assertEquals(3 * 3 + 4 * 4 + 5 * 5, ListAlgorithms
        .getSumOfAreasOfAllIntersecting(INCREASING_ENCLOSING,
            new Rectangle(new Point(3, 3), 10, 10)));
  }

  @Test
  public void listGetAreaMap() {
    Map<Rectangle, Integer> result = ListAlgorithms.getAreaMap(INCREASING_ENCLOSING);
    Map<Rectangle, Integer> expected = new HashMap<>();
    expected.put(INCREASING_ENCLOSING.get(0), 1 * 1);
    expected.put(INCREASING_ENCLOSING.get(1), 2 * 2);
    expected.put(INCREASING_ENCLOSING.get(2), 3 * 3);
    expected.put(INCREASING_ENCLOSING.get(3), 4 * 4);
    expected.put(INCREASING_ENCLOSING.get(4), 5 * 5);

    assertEquals(expected.keySet().size(), result.keySet().size());

    for (Rectangle r : expected.keySet()) {
      assertTrue(containsMatchingPair(result, r, expected.get(r)));
    }

  }

  private boolean containsMatchingPair(Map<Rectangle, Integer> toCheck, Rectangle key,
      Integer value) {
    for (Rectangle r : toCheck.keySet()) {
      if (equalRectangles(r, key) && toCheck.get(r).equals(value)) {
        return true;
      }
    }
    return false;
  }

  //=====================================
  // 4. Tests for StreamAlgorithms class.
  //=====================================

  @Test
  public void streamTranslate() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_TRANSLATED, StreamAlgorithms
            .translate(VARIOUS_RECTANGLES.stream(), new Point(10, 100)).collect(toList())));
  }

  @Test
  public void streamScale() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_SCALED,
        StreamAlgorithms.scale(VARIOUS_RECTANGLES.stream(), 10).collect(toList())));
  }

  @Test
  public void streamTranslateAndScale() {
    assertTrue(equalRectangleLists(
        VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED, StreamAlgorithms.scale(
            StreamAlgorithms.translate(VARIOUS_RECTANGLES.stream(), new Point(10, 100)),
            10).collect(toList())));
  }

  @Test
  public void streamGetBottomLeft() {
    List<Point> bottomLeft = StreamAlgorithms.getBottomLeftPoints(VARIOUS_RECTANGLES.stream())
        .collect(toList());
    List<Point> expected = Arrays.asList(
        new Point(10, 20),
        new Point(15, 25),
        new Point(0, 1000),
        new Point(2000, 2001));
    assertTrue(equalPointLists(bottomLeft, expected));
  }

  @Test
  public void streamGetAllIntersecting1() {
    assertTrue(equalRectangleLists(INCREASING_ENCLOSING, StreamAlgorithms
        .getAllIntersecting(INCREASING_ENCLOSING.stream(), INCREASING_ENCLOSING.get(2))
        .collect(toList())));
  }

  @Test
  public void streamGetAllIntersecting2() {
    assertTrue(equalRectangleLists(new ArrayList<Rectangle>(), StreamAlgorithms
        .getAllIntersecting(new ArrayList<Rectangle>().stream(), new Rectangle(0, 0))
        .collect(toList())));
  }

  @Test
  public void streamGetAllIntersecting3() {
    Rectangle candidate = new Rectangle(new Point(3, 3), 100, 100);
    List<Rectangle> expected = Arrays
        .asList(INCREASING_ENCLOSING.get(2), INCREASING_ENCLOSING.get(3),
            INCREASING_ENCLOSING.get(4));
    assertTrue(equalRectangleLists(expected,
        StreamAlgorithms.getAllIntersecting(INCREASING_ENCLOSING.stream(), candidate)
            .collect(toList())));
  }

  @Test
  public void streamGetAllWithBiggerAreaThan1() {
    Rectangle candidate = new Rectangle(new Point(3, 3), 100, 100);
    List<Rectangle> expected = new ArrayList<>();
    assertTrue(equalRectangleLists(expected,
        StreamAlgorithms.getAllWithBiggerAreaThan(INCREASING_ENCLOSING.stream(), candidate).collect(
            toList())));
  }

  @Test
  public void streamGetAllWithBiggerAreaThan2() {
    Rectangle candidate = new Rectangle(new Point(100, 100), 1, 1);
    List<Rectangle> expected = Arrays
        .asList(INCREASING_ENCLOSING.get(1), INCREASING_ENCLOSING.get(2),
            INCREASING_ENCLOSING.get(3), INCREASING_ENCLOSING.get(4));
    assertTrue(equalRectangleLists(expected,
        StreamAlgorithms.getAllWithBiggerAreaThan(INCREASING_ENCLOSING.stream(), candidate)
            .collect(toList())));
  }

  @Test
  public void streamFindLargestArea() {
    assertEquals(5 * 5, StreamAlgorithms.findLargestArea(INCREASING_ENCLOSING.stream()));
    assertEquals(1000 * 1000, StreamAlgorithms.findLargestArea(VARIOUS_RECTANGLES.stream()));
    assertEquals(1000 * 1000,
        StreamAlgorithms.findLargestArea(VARIOUS_RECTANGLES_TRANSLATED.stream()));
    assertEquals(10000 * 10000,
        StreamAlgorithms.findLargestArea(VARIOUS_RECTANGLES_SCALED.stream()));
    assertEquals(10000 * 10000,
        StreamAlgorithms.findLargestArea(VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED.stream()));
    assertEquals(0, StreamAlgorithms.findLargestArea(new ArrayList<Rectangle>().stream()));
  }

  @Test
  public void streamFindMaxHeight() {
    assertEquals(5, StreamAlgorithms.findMaxHeight(INCREASING_ENCLOSING.stream()));
    assertEquals(1000, StreamAlgorithms.findMaxHeight(VARIOUS_RECTANGLES.stream()));
    assertEquals(1000, StreamAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_TRANSLATED.stream()));
    assertEquals(10000, StreamAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_SCALED.stream()));
    assertEquals(10000,
        StreamAlgorithms.findMaxHeight(VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED.stream()));
  }

  @Test
  public void streamGetSumOfAreas() {
    assertEquals(1 * 1 + 2 * 2 + 3 * 3 + 4 * 4 + 5 * 5,
        StreamAlgorithms.getSumOfAreas(INCREASING_ENCLOSING.stream()));
  }

  @Test
  public void streamGetSumOfAreasOfAllIntersecting() {
    assertEquals(0, StreamAlgorithms.getSumOfAreasOfAllIntersecting(INCREASING_ENCLOSING.stream(),
        new Rectangle(new Point(100, 100), 10, 10)));
    assertEquals(3 * 3 + 4 * 4 + 5 * 5, StreamAlgorithms
        .getSumOfAreasOfAllIntersecting(INCREASING_ENCLOSING.stream(),
            new Rectangle(new Point(3, 3), 10, 10)));
  }

  @Test
  public void streamGetAreaMap() {
    Map<Rectangle, Integer> result = StreamAlgorithms.getAreaMap(INCREASING_ENCLOSING.stream());
    Map<Rectangle, Integer> expected = new HashMap<>();
    expected.put(INCREASING_ENCLOSING.get(0), 1 * 1);
    expected.put(INCREASING_ENCLOSING.get(1), 2 * 2);
    expected.put(INCREASING_ENCLOSING.get(2), 3 * 3);
    expected.put(INCREASING_ENCLOSING.get(3), 4 * 4);
    expected.put(INCREASING_ENCLOSING.get(4), 5 * 5);

    assertEquals(expected.keySet().size(), result.keySet().size());

    for (Rectangle r : expected.keySet()) {
      assertTrue(containsMatchingPair(result, r, expected.get(r)));
    }

  }

  //==============================================================================
  // 5. Tests to check equivalence of ListAlgorithms and StreamAlgorithms classes.
  //==============================================================================

  private static final List<List<Rectangle>> RECTANGLE_LISTS = Arrays.asList(
      INCREASING_ENCLOSING, VARIOUS_RECTANGLES, VARIOUS_RECTANGLES_SCALED,
      VARIOUS_RECTANGLES_TRANSLATED, VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED);

  @Test
  public void compareListStreamTranslate() {

    // You should look up what "forEach" does in the Java documentation

    RECTANGLE_LISTS.forEach(l ->
        assertTrue(equalRectangleLists(
            ListAlgorithms.translate(l, new Point(20, 20)),
            StreamAlgorithms.translate(l.stream(), new Point(20, 20)).collect(toList())
        )));
  }

  @Test
  public void compareListStreamScale() {
    RECTANGLE_LISTS.forEach(l ->
        assertTrue(equalRectangleLists(
            ListAlgorithms.scale(l, 42),
            StreamAlgorithms.scale(l.stream(), 42).collect(toList())
        )));
  }

  @Test
  public void compareListStreamGetBottomLeft() {
    RECTANGLE_LISTS.forEach(l ->
        assertTrue(equalPointLists(
            ListAlgorithms.getBottomLeftPoints(l),
            StreamAlgorithms.getBottomLeftPoints(l.stream()).collect(toList())
        )));
  }

  @Test
  public void compareListStreamGetAllIntersecting() {
    RECTANGLE_LISTS.forEach(l -> {
      Rectangle intersectionCandidate = new Rectangle(new Point(2, 2), 1000, 2);
      assertTrue(equalRectangleLists(
          ListAlgorithms.getAllIntersecting(l, intersectionCandidate),
          StreamAlgorithms.getAllIntersecting(l.stream(), intersectionCandidate).collect(toList())
      ));
    });
  }

  @Test
  public void compareListStreamGetAllWithBiggerAreaThan() {
    RECTANGLE_LISTS.forEach(l -> {
      Rectangle areaCandidate = new Rectangle(new Point(2, 2), 60, 2);
      assertTrue(equalRectangleLists(
          ListAlgorithms.getAllWithBiggerAreaThan(l, areaCandidate),
          StreamAlgorithms.getAllWithBiggerAreaThan(l.stream(), areaCandidate).collect(toList())
      ));
    });
  }

  @Test
  public void compareListStreamFindLargestArea() {
    RECTANGLE_LISTS.forEach(l -> {
      assertEquals(
          ListAlgorithms.findLargestArea(l),
          StreamAlgorithms.findLargestArea(l.stream())
      );
    });
  }

  @Test
  public void compareListStreamFindMaxHeight() {
    RECTANGLE_LISTS.forEach(l -> {
      assertEquals(
          ListAlgorithms.findMaxHeight(l),
          StreamAlgorithms.findMaxHeight(l.stream())
      );
    });
  }

  @Test
  public void compareListStreamGetSumOfAreas() {
    RECTANGLE_LISTS.forEach(l -> {
      assertEquals(
          ListAlgorithms.getSumOfAreas(l),
          StreamAlgorithms.getSumOfAreas(l.stream())
      );
    });
  }

  @Test
  public void compareListStreamGetSumOfAreasOfAllIntersecting() {
    RECTANGLE_LISTS.forEach(l -> {
      Rectangle intersectionCandidate = new Rectangle(new Point(10, 10), 10, 10);
      assertEquals(
          ListAlgorithms.getSumOfAreasOfAllIntersecting(l, intersectionCandidate),
          StreamAlgorithms.getSumOfAreasOfAllIntersecting(l.stream(), intersectionCandidate)
      );
    });
  }

  //======================================
  // 6. Tests for intersecting rectangles.
  //======================================

  @Test
  public void rectangleIntersection1() {
    assertTrue(
        equalRectangles(
            new Rectangle(new Point(10, 10), 5, 5),
            new Rectangle(new Point(0, 0), 15, 15).intersection(
                new Rectangle(new Point(10, 10), 15, 15)).get()
        ));
  }

  @Test
  public void rectangleIntersection2() {
    assertTrue(
        equalRectangles(
            new Rectangle(new Point(15, 15), 0, 0),
            new Rectangle(new Point(0, 0), 15, 15).intersection(
                new Rectangle(new Point(15, 15), 15, 15)).get()
        ));
  }

  @Test
  public void rectangleIntersection3() {
    assertFalse(
        new Rectangle(new Point(0, 0), 15, 15)
            .intersection(new Rectangle(new Point(16, 16), 15, 15)).isPresent());
  }

  @Test
  public void streamIntersectAll1() {
    assertTrue(
        equalRectangles(INCREASING_ENCLOSING.get(0),
            StreamAlgorithms.intersectAll(INCREASING_ENCLOSING.stream()).get())
    );
  }

  @Test
  public void streamIntersectAll2() {
    assertFalse(
        StreamAlgorithms.intersectAll(VARIOUS_RECTANGLES_TRANSLATED_AND_SCALED.stream())
            .isPresent());
  }

  @Test
  public void streamIntersectAllSingle() {
    List<Rectangle> rects = new ArrayList<>();
    rects.add(new Rectangle(10, 10));
    assertTrue(equalRectangles(new Rectangle(10, 10),
        StreamAlgorithms.intersectAll(rects.stream()).get()));
  }

  //===============================
  // 7. Tests for streams of cubes.
  //===============================

  @Test
  public void cubeSupplier() {
    final CubeSupplier cubeSupplier = new CubeSupplier();
    for (int i = 1; i <= 1000; i++) {
      assertEquals(i * i * i, (int) cubeSupplier.get());
    }
  }

  @Test
  public void cubeStream() {
    assertTrue(CubeSupplier.cubeStream().anyMatch(item -> item == 1000));
  }

  @Test(expected = NoSuchElementException.class)
  public void cubeStreamException() {
    // 999 is not a perfect cube, so we expect the cube stream to be generated until
    // the largest positive int value has been exceeded, in which case an exception
    // should be thrown.
    CubeSupplier.cubeStream().anyMatch(item -> item == 999);
  }

  @Test
  public void boundedCubeStream() {
    assertEquals((int) CubeSupplier.boundedCubeStream(0, 10)
        .reduce(0, Integer::sum), 3025);
  }

  @Test
  public void boundedCubeStreamNoException() {
    assertFalse(CubeSupplier.boundedCubeStream(0, 50).anyMatch(item -> item == 999));
  }


  @Test
  public void palindromes() {
    assertEquals(Arrays.asList(1, 8, 343, 1331, 1030301, 1367631),
        CubeSupplier.palindromicCubes(0, 1000).collect(Collectors.toList()));
  }

  //==============================
  // 8. Tests for string prefixes.
  //==============================

  private static final List<String> strings = Arrays.asList(
      "Java thinking",
      "Haskell programming",
      "Java programming",
      "Haskell breathing",
      "Java programming",
      "Java and Haskell: better together?",
      "Java programming",
      "Smalltalk programming",
      "C++ programming");

  @Test
  public void countStringsStartingWithPrefix() {
    assertEquals(0, StringPrefixes.countStringsStartingWithPrefix(
        Stream.empty(), "Hello"));
    assertEquals(5, StringPrefixes.countStringsStartingWithPrefix(
        strings.stream(), "Java"));
    assertEquals(2, StringPrefixes.countStringsStartingWithPrefix(
        strings.stream(), "Haskell"));
    assertEquals(0, StringPrefixes.countStringsStartingWithPrefix(
        strings.stream(), "C#"));
  }

  @Test
  public void emphasiseFirstStringStartingWithPrefix() {
    assertEquals("N/A", StringPrefixes.emphasiseFirstStringStartingWithPrefix(
        Stream.empty(), "Hello"));
    assertEquals("*Java* thinking", StringPrefixes.emphasiseFirstStringStartingWithPrefix(
        strings.stream(), "Java"));
    assertEquals("*Haskell* programming", StringPrefixes.emphasiseFirstStringStartingWithPrefix(
        strings.stream(), "Haskell"));
    assertEquals("N/A", StringPrefixes.emphasiseFirstStringStartingWithPrefix(
        strings.stream(), "C#"));
  }

  @Test
  public void distinctStringsStartingWithPrefix() {
    assertEquals(new ArrayList<>(), StringPrefixes.distinctStringsStartingWithPrefix(
        Stream.empty(), "Hello"));
    assertEquals(Arrays.asList(
            "Java thinking",
            "Java programming",
            "Java and Haskell: better together?"),
        StringPrefixes.distinctStringsStartingWithPrefix(strings.stream(), "Java"));
    assertEquals(Arrays.asList(
            "Haskell programming",
            "Haskell breathing"),
        StringPrefixes.distinctStringsStartingWithPrefix(strings.stream(), "Haskell"));
  }

  // ============================
  // Additional tests for Point.
  // ============================

  //checks that creating a point with a negative coordinate produces the correct error
  @Test(expected = NumberFormatException.class)
  public void pointErrorsWithNegative() {
    new Point(-1, 0);
  }

  //checks that trying to add a point with a negative coordinate produces the correct error
  @Test(expected = NumberFormatException.class)
  public void pointAddErrorsWithNegative() {
    new Point(1, 0).add(new Point(-1, 0));
  }

  //checks that trying to set a points x to be a negative number produces the correct error
  @Test(expected = NumberFormatException.class)
  public void pointSetXErrorsWithNegative() {
    new Point(1, 0).setX(-1);
  }

  //checks that trying to set a points y to be a negative number produces the correct error
  @Test(expected = NumberFormatException.class)
  public void pointSetYErrorsWithNegative() {
    new Point(1, 0).setY(-1);
  }

  // ================================
  // Additional tests for Rectangle.
  // ================================

  //Checks that creating a rectangle with negative coordinates produces the correct error
  @Test(expected = NumberFormatException.class)
  public void rectangleErrorWithNegative() {
    new Rectangle(-5, 2);
  }

  //checks that 2 rectangles of size 0x0 intersect
  @Test
  public void rectangleNoAreaIntersect() {
    assertTrue(new Rectangle(0, 0).intersects(new Rectangle(0, 0)));
  }

  //checks that if 1 rectangle completely contains the other they intersect
  @Test
  public void rectangleContainedIntersects() {
    assertTrue(
        new Rectangle(new Point(1, 1), 2, 2).intersects(new Rectangle(5, 5)));
    assertTrue(
        new Rectangle(5, 5).intersects(new Rectangle(new Point(1, 1), 2, 2)));
  }

  //Checks if rectangles which only intersect at 1 corner intersect
  @Test
  public void rectangleIntersectCorner() {
    assertTrue(new Rectangle(1, 1).intersects(new Rectangle(new Point(1, 1), 1, 1)));
  }

  // =====================================
  // Additional tests for ListAlgorithms.
  // =====================================

  //check that scaling by a factor of 0 works
  @Test
  public void listScaleZero() {
    List<Rectangle> rects = new ArrayList<>();
    rects.add(new Rectangle(new Point(1, 1), 2, 2));
    List<Rectangle> rectsNew = new ArrayList<>();
    rectsNew.add(new Rectangle(new Point(1, 1), 0, 0));
    assertTrue(equalRectangleLists(ListAlgorithms.scale(rects, 0), rectsNew));
  }

  // =======================================
  // Additional tests for StreamAlgorithms.
  // =======================================

  //check that scaling by a factor of 0 works
  @Test
  public void streamScaleZero() {
    List<Rectangle> rects = new ArrayList<>();
    rects.add(new Rectangle(new Point(1, 1), 2, 2));
    List<Rectangle> rectsNew = new ArrayList<>();
    rectsNew.add(new Rectangle(new Point(1, 1), 0, 0));
    assertTrue(
        equalRectangleLists(StreamAlgorithms.scale(rects.stream(), 0).collect(toList()), rectsNew));
  }

}
