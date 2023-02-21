package spreadsheet;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.junit.Assert.assertThat;

import common.api.CellLocation;
import java.util.HashSet;
import java.util.Set;
import org.junit.Test;

public class TestFindCellReferences {

  @Test
  public void testNumber() throws Exception {
    assertThat(dependencies("4"), empty());
    assertThat(dependencies("1.5"), empty());
    assertThat(dependencies("6.8"), empty());
  }

  @Test
  public void testCellLocation() throws Exception {
    assertThat(dependencies("a1"), containsInAnyOrder("a1"));
    assertThat(dependencies("bc15"), containsInAnyOrder("bc15"));
  }

  @Test
  public void testBinaryOperator() throws Exception {
    assertThat(dependencies("2 + 5"), empty());
    assertThat(dependencies("a1 + 5"), containsInAnyOrder("a1"));
    assertThat(dependencies("a1 + c4"), containsInAnyOrder("a1", "c4"));
    assertThat(dependencies("a1 + c4 * w7"), containsInAnyOrder("a1", "c4", "w7"));
    assertThat(dependencies("a1 + a1 * a1"), containsInAnyOrder("a1"));
  }

  private Set<String> dependencies(String input) throws Exception {
    Set<CellLocation> cells = new HashSet<>();
    Parser.parse(input).findCellReferences(cells);

    Set<String> result = new HashSet<>();
    for (CellLocation cell : cells) {
      result.add(cell.toString());
    }
    return result;
  }
}
