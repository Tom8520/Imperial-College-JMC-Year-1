package common.gui;

import common.api.BasicSpreadsheet;
import common.api.CellLocation;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.table.AbstractTableModel;
import spreadsheet.InvalidSyntaxException;

/**
 * The data model for the spreadsheet, sitting atop a BasicSpreadsheet.
 */
public class SpreadsheetTableModel extends AbstractTableModel {

  private final int numberOfRows;
  private final int numberOfColumns;

  private final BasicSpreadsheet spreadsheet;

  private final JComponent parent;

  /**
   * Constructs a spreadsheet table model.
   *
   * @param parent          The top panel of the spreadsheet window, used for positioning error
   *                        messages.
   * @param spreadsheet     The spreadsheet this GUI is tracking.
   * @param numberOfRows    Number of rows in the spreadsheet.
   * @param numberOfColumns Number of columns in the spreadsheet.
   */
  protected SpreadsheetTableModel(
      JComponent parent, BasicSpreadsheet spreadsheet, int numberOfRows, int numberOfColumns) {
    this.parent = parent;
    this.spreadsheet = spreadsheet;
    this.numberOfRows = numberOfRows;
    this.numberOfColumns = numberOfColumns + 1;
  }

  public Class<?> getColumnClass(int column) {
    return (column == 0) ? String.class : CellView.class;
  }

  @Override
  public String getColumnName(int column) {
    return (column == 0) ? "" : CellLocation.getColumnName(column - 1);
  }

  @Override
  public boolean isCellEditable(int row, int column) {
    return column != 0;
  }

  @Override
  public int getRowCount() {
    return numberOfRows;
  }

  @Override
  public int getColumnCount() {
    return numberOfColumns;
  }

  @Override
  public Object getValueAt(int row, int column) {
    assert row >= 0 && column >= 0;

    if (column == 0) {
      return Integer.toString(row + 1);
    }

    CellLocation location = new CellLocation(row, column - 1);
    return new CellView(
        this.spreadsheet.getCellExpression(location), this.spreadsheet.getCellDisplay(location));
  }

  @Override
  public void setValueAt(Object value, int row, int column) {
    assert value != null && row >= 0 && column > 0;

    CellLocation location = new CellLocation(row, column - 1);
    CellView data = (CellView) value;
    String expression = data.getExpression();

    try {
      this.spreadsheet.setCellExpression(location, expression);
    } catch (InvalidSyntaxException e) {
      JOptionPane.showMessageDialog(parent, "Syntax error");
    }

    // Let things know every cell in the table may have updated.
    // (Possible extension: change this to track only those cells that actually *have* updated.)
    fireTableDataChanged();
  }
}
