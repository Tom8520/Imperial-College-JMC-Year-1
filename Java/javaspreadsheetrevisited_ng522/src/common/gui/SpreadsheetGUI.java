package common.gui;

import common.api.BasicSpreadsheet;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumn;

/**
 * The graphical user interface class for the Spreadsheet.
 */
public final class SpreadsheetGUI extends JFrame {

  private static final Dimension FRAME_SIZE = new Dimension(640, 480);
  private static final int FIRST_COLUMN_WIDTH = 35;

  public SpreadsheetGUI(BasicSpreadsheet spreadsheet, int numberOfRows, int numberOfColumns) {
    super("Spreadsheet");

    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    setLayout(new BorderLayout());

    SpreadsheetTableModel tableModel =
        new SpreadsheetTableModel(this.rootPane, spreadsheet, numberOfRows, numberOfColumns);
    JTable table = setupTable(tableModel);

    this.setupLocationAndSize(table);

    add(new JScrollPane(table), BorderLayout.CENTER);
  }

  private static JTable setupTable(SpreadsheetTableModel tableModel) {
    JTable table = new JTable(tableModel);
    table.setAutoCreateColumnsFromModel(false);
    table.setShowGrid(true);
    table.setGridColor(Color.LIGHT_GRAY);

    table.setDefaultEditor(CellView.class, new SpreadsheetCellEditor());
    table.setDefaultRenderer(CellView.class, new SpreadsheetCellRenderer(table.getFont()));

    setupFirstColumn(table.getColumnModel().getColumn(0));

    table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    table.getTableHeader().setReorderingAllowed(false);
    return table;
  }

  private static void setupFirstColumn(TableColumn firstColumn) {
    firstColumn.setMinWidth(FIRST_COLUMN_WIDTH);
    firstColumn.setPreferredWidth(FIRST_COLUMN_WIDTH);
  }

  private void setupLocationAndSize(JTable table) {
    table.setPreferredScrollableViewportSize(FRAME_SIZE);
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    this.setLocation(
        (screenSize.width - FRAME_SIZE.width) / 2, (screenSize.height - FRAME_SIZE.height) / 2);
  }

  /**
   * Starts the spreadsheet GUI.
   */
  public void start() {
    javax.swing.SwingUtilities.invokeLater(
        () -> {
          pack();
          setVisible(true);
        });
  }
}
