package common.gui;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.util.EventObject;
import javax.swing.AbstractCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.TableCellEditor;

/**
 * A cell editor that implements spreadsheet-like cell editing.
 *
 * <p>The editor is aware of cell views, supports double-clicking to enable editing, and fills in
 * existing expressions when invoked.
 */
public class SpreadsheetCellEditor extends AbstractCellEditor implements TableCellEditor {

  private final JTextField textField = new JTextField();

  @Override
  public Object getCellEditorValue() {
    return new CellView(textField.getText(), textField.getText());
  }

  @Override
  public boolean isCellEditable(EventObject e) {
    /* The setFocusable calls here try to work around a problem where, if we are editing a cell by
    keyboard, the cell will focus immediately on the user typing the first keystroke, and
    cause unwanted selection behaviour.  This just disables focus outright unless we're double
    clicking, which fixes this but makes the selection behaviour suboptimal on keyboard editing.

    More sophisticated solutions are left, for now, as an extension. */
    this.textField.setFocusable(false);

    if (e instanceof MouseEvent) {
      if (((MouseEvent) e).getClickCount() >= 2) {
        this.textField.setFocusable(true);
        return true;
      }
      return false;
    }
    return true;
  }

  @Override
  public Component getTableCellEditorComponent(
      JTable table, Object value, boolean isSelected, int row, int column) {
    textField.setText("");
    if (value != null) {
      String expr = ((CellView) value).getExpression();
      if (!expr.isEmpty()) {
        textField.setText(expr);
      }
    }

    return textField;
  }
}
