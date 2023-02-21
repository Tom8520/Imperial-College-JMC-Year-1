package common.api;

import java.util.Set;

/**
 * Common interface implemented by each AST node.
 *
 * <p>In addition to the two methods listed here, you should override the `toString` method.
 */
public interface Expression {

  /**
   * Evaluate the expression as a double value.
   *
   * <p>The provided `EvaluationContext` may be used to lookup the value of other cells.
   */
  double evaluate(EvaluationContext context);

  /**
   * Find all cell locations referenced by this expression (and its subexpressions).
   *
   * <p>The cell locations should be added to the `dependencies` set.
   */
  void findCellReferences(Set<CellLocation> dependencies);
}
