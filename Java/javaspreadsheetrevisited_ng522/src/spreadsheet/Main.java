package spreadsheet;

import common.gui.SpreadsheetGUI;

public class Main {

  /**
   * Starting point of the program.
   *
   * <p>You shouldn't have to modify this.
   */
  public static void main(String[] args) {
    new SpreadsheetGUI(new Spreadsheet(), 30, 8).start();
  }
}
