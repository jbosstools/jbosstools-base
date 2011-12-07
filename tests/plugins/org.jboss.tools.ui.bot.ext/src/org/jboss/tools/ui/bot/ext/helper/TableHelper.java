/*******************************************************************************
 * Copyright (c) 2007-2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.helper;

import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.StringResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;

/**
 * Helper for Table widget 
 * @author vlado pakan
 *
 */
public class TableHelper {
	
	/**
	 * Returns table selection text on the selectionRow row and selectionColumn column of selection
	 * Working also in case when table has no columns added (table.columnCount == 0) 
	 * @param table
	 * @param selectionRow
	 * @param selectionColumn
	 * @return
	 */
  public static String getSelectionText(final SWTBotTable table,
      final int selectionRow, final int selectionColumn) {
    table.setFocus();
    return UIThreadRunnable.syncExec(new StringResult() { 
      @Override
      public String run() {
        return table.widget.getSelection()[selectionRow].getText(selectionColumn);
      }
    });
  }
  /**
   * Returns table selection text on the selectionRow row and first column of selection
   * Working also in case when table has no columns added (table.columnCount == 0) 
   * @param table
   * @param selectionRow
   * @return
   */
  public static String getSelectionText(final SWTBotTable table, final int selectionRow) {
    return TableHelper.getSelectionText(table, selectionRow , 0);
  }
  /**
   * Returns table selection text on the first row and first column of selection
   * Working also in case when table has no columns added (table.columnCount == 0) 
   * @param table
   * @return
   */
  public static String getSelectionText(final SWTBotTable table) {
    return getSelectionText(table, 0 , 0);
  }
}
