package common.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * A renderer for spreadsheet cells.
 */
public class SpreadsheetCellRenderer extends DefaultTableCellRenderer {

  private static Border focusCellHighlightBorder =
      UIManager.getBorder("Table.focusCellHighlightBorder");

  private static Color focusCellForeground = UIManager.getColor("Table.focusCellForeground");

  private static Color focusCellBackground = UIManager.getColor("Table.focusCellBackground");

  private static Color headerBackground = UIManager.getColor("TableHeader.background");

  private static Color tableBackground = UIManager.getColor("Table.background");

  protected SpreadsheetCellRenderer(Font font) {
    super();
    this.setFont(font);

    if (focusCellHighlightBorder == null) {
      focusCellHighlightBorder = new LineBorder(Color.GRAY, 2);
    }

    if (focusCellForeground == null) {
      focusCellForeground = Color.WHITE;
    }

    if (focusCellBackground == null) {
      focusCellBackground = Color.WHITE;
    }

    if (headerBackground == null) {
      headerBackground = new Color(238, 238, 238);
    }

    if (tableBackground == null) {
      tableBackground = Color.WHITE;
    }
  }

  @Override
  public Component getTableCellRendererComponent(
      JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
    setForeground(Color.BLACK);
    setHorizontalAlignment(JTextField.RIGHT);

    if (value != null) {
      CellView cell = (CellView) value;
      this.setText(cell.getDisplay());
    } else {
      setValue("");
    }

    if (hasFocus) {
      setBorder(focusCellHighlightBorder);
      setForeground(focusCellForeground);
      setBackground(focusCellBackground);
    } else {
      setBorder(noFocusBorder);
      setBackground(tableBackground);
    }

    return this;
  }

  @Override
  protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
  }
}
