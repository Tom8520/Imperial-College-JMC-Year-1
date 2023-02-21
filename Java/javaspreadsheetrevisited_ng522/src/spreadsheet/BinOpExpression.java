package spreadsheet;

import common.api.CellLocation;
import common.api.EvaluationContext;
import common.api.Expression;
import common.lexer.Token.Kind;
import java.util.Set;

public class BinOpExpression implements Expression {

  private final Kind operation;
  private final Expression leftOperand;
  private final Expression rightOperand;

  BinOpExpression(Kind operation, Expression leftOperand, Expression rightOperand) {
    this.operation = operation;
    this.leftOperand = leftOperand;
    this.rightOperand = rightOperand;
  }

  @Override
  public double evaluate(EvaluationContext context) {
    double left = leftOperand.evaluate(context);
    double right = rightOperand.evaluate(context);

    switch (operation) {
      case PLUS -> {
        return left + right;
      }
      case MINUS -> {
        return left - right;
      }
      case STAR -> {
        return left * right;
      }
      case SLASH -> {
        return left / right;
      }
      case CARET -> {
        return Math.pow(left, right);
      }
      default -> {
        throw new UnsupportedOperationException();
      }
    }
  }

  @Override
  public void findCellReferences(Set<CellLocation> dependencies) {
    leftOperand.findCellReferences(dependencies);
    rightOperand.findCellReferences(dependencies);
  }

  @Override
  public String toString() {
    String operation;

    switch (this.operation) {
      case STAR -> operation = "*";
      case PLUS -> operation = "+";
      case MINUS -> operation = "-";
      case SLASH -> operation = "/";
      case CARET -> operation = "^";
      default -> throw new UnsupportedOperationException();
    }
    return "(" + leftOperand.toString() + " " + operation + " " + rightOperand.toString() + ")";
  }
}
