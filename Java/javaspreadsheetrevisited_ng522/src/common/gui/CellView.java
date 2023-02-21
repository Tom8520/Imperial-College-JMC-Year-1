package common.gui;

/**
 * The GUI's view of one cell.
 *
 * <p>This class isn't directly usable as a cell representation, as it only contains enough
 * behaviour for the GUI to be able to render a cell's value when not editing, and expression when
 * editing; it is also immutable.
 */
class CellView {

  private final String expression;
  private final String display;

  /**
   * Constructs a cell view.
   *
   * @param expression the expression string (can be empty, but should not be null).
   * @param display    the display string (can be empty, but should not be null).
   */
  CellView(String expression, String display) {
    this.expression = expression;
    this.display = display;
  }

  /**
   * @return the current expression of this cell.
   */
  String getExpression() {
    return this.expression;
  }

  /**
   * @return a string that should be displayed in the cell whenever it is not being edited.
   */
  String getDisplay() {
    return this.display;
  }
}
