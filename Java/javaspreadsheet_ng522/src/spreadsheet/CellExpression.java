package spreadsheet;

import common.api.CellLocation;
import common.api.EvaluationContext;
import common.api.Expression;
import java.util.Set;

public class CellExpression implements Expression {

  private final String cell;

  CellExpression(String cell) {
    this.cell = cell;
  }


  @Override
  public double evaluate(EvaluationContext context) {
    return context.getCellValue(new CellLocation(cell));
  }

  @Override
  public void findCellReferences(Set<CellLocation> dependencies) {
    dependencies.add(new CellLocation(cell));
  }

  @Override
  public String toString() {
    return cell;
  }
}
