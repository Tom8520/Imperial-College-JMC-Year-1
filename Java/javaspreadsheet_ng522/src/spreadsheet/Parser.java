package spreadsheet;

import common.api.Expression;
import common.lexer.InvalidTokenException;
import common.lexer.Lexer;
import common.lexer.Token;
import java.util.Stack;

public class Parser {

  /**
   * Parse a string into an Expression.
   *
   * <p>DO NOT CHANGE THE SIGNATURE. The test suite depends on this.
   */
  static Expression parse(String input) throws InvalidSyntaxException, InvalidTokenException {
    Stack<Expression> operands = new Stack<>();
    Stack<Token> operations = new Stack<>();

    Lexer lex = new Lexer(input);

    while (true) {
      Token token = lex.nextToken();

      if (token == null) {
        break;
      }

      switch (token.kind) {
        case NUMBER -> {
          operands.push(new NumberExpression(token.numberValue));
        }
        case CELL_LOCATION -> {
          operands.push(new CellExpression(token.cellLocationValue.toString()));
        }
        default -> {
          while (!operations.isEmpty()
              && Token.precedence(operations.peek().kind) >= Token.precedence(token.kind)) {

            if (operations.peek().kind == token.kind && Token.associativity(token.kind)
                .equals("RIGHT")) {
              break;
            }
            Token operator = operations.pop();

            Expression right = operands.pop();
            Expression left = operands.pop();

            operands.push(new BinOpExpression(operator.kind, left, right));
          }
          operations.push(token);
        }
      }
    }

    while (!operations.isEmpty()) {
      Token operator = operations.pop();

      Expression right = operands.pop();
      Expression left = operands.pop();

      operands.push(new BinOpExpression(operator.kind, left, right));
    }

    return operands.pop();
  }
}
