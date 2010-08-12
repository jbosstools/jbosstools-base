/*******************************************************************************

 * Copyright (c) 2007-2010 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swt.widgets.Table;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.Timing;

/**
 * Extends Table Component
 * @author vlado pakan
 *
 */
public class SWTBotTableExt extends AbstractSWTBotControl<Table> {

  private SWTBotTable swtBotTable = null;
  
  private SWTBotTableExt(Table table) throws WidgetNotFoundException {
    super(table);
  }
  
  public SWTBotTableExt(SWTBotTable swtBotTable) throws WidgetNotFoundException {
    this(swtBotTable.widget);
    this.swtBotTable = swtBotTable;
  }
  /**
   * Sets value of table cell editable via Text Cell Editor on position specified by row and column
   * @param newValue - new value of the cell
   * @param row - zero based row index
   * @param column - zero based column index
   * @param oldValue - old value of the cell
   * @param bot - bot containing table
   */
  public void setTableCellWithTextEditorText(String newValue,
    int row , int column,
    String oldValue,
    SWTBot bot){
    
    swtBotTable.click(row, column);
    bot.sleep(Timing.time1S());
    bot.text(oldValue, 0).setText(newValue);
    
  }
  
}
