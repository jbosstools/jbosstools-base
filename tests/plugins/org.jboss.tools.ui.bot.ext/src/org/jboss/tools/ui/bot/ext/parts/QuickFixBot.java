 /*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/

package org.jboss.tools.ui.bot.ext.parts;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.jboss.tools.ui.bot.ext.helper.ContextMenuHelper;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

/**
 * This provides working Content assist functionality. 
 * SWTBot (2.0.0#467) functionality provided in SWTEclipseEditor doesn't work (at least on GTK linux) 
 * @author jpeterka
 *
 */
public class QuickFixBot {
	/**
	 * Performs content assist for given editor and string
	 * 
	 * @param editor
	 * @param text
	 */

	Logger log = Logger.getLogger(QuickFixBot.class);
	SWTBotEditorExt editor;
	SWTBot bot;

	// ------------------------------------------------------------
	// Constructor
	// ------------------------------------------------------------

	 /**
   * Basic constructor
   */
  public QuickFixBot(SWTBotEditorExt editor) {
    this.editor = editor;
    this.bot = editor.bot();
  }


	// ------------------------------------------------------------
	// Public
	// ------------------------------------------------------------
	/**
	 * Use selected Quick Fix with given Quick Fix text 
	 */
	public void useQuickFix(String text) {
		SWTBotTable table = getQuickFixTable(openQuickFixList());
		List<String> items = getTableItems(table);		
		if (items.contains(text)) {
			final int index = items.indexOf(text);
			seletctQFTableItem(table, index);
		} else {
			fail("Quick Fix doens't contain Quick Fix text");
		}
	}
	/**
	 * Use selected Quick Fix with given Quick Fix index 
	 */
	public void useQuickFix(int index) {
		SWTBotTable table = getQuickFixTable(openQuickFixList());
		seletctQFTableItem(table, index);
	}

	/**
	 * Logs Quick Fix list contents, usual for debug purposes
	 * 
	 * @param text
	 */
	public void logQuickFixList() {
		List<String> list = getQuickFixList();
		log.info("Quick Fix item list: " + list.size() + " item(s)");
		for (int i = 0; i < list.size(); i++) {
			log.info("Item i:" + list.get(i));
		}
	}
	/**
	 * Returns Quick Fix list
	 * @return
	 */
  public List<String>getQuickFixList() {
    List<String> result = null;
    SWTBotShell shell = openQuickFixList();
    SWTBotTable table = getQuickFixTable(shell);
    result = getTableItems(table);
    shell.close();
    
    return result;
  }
  /**
   * Logs Quick Fix list contents, when Quick Fixt window is already opened
   * @param shellsBefore - list of shells before Content Assist was invoked
   * @param shellsAfter - list of shells after Content Assist was invoked
   * @param closeShell
   * @return
   */
  public List<String>getQuickFixList(SWTBotShell[] shellsBefore, SWTBotShell[] shellsAfter, boolean closeShell) {
    List<String> result = null;
    SWTBotShell caShell = getQuickFixShell(shellsBefore, shellsAfter);
    SWTBotTable caTable = getQuickFixTable(caShell);
    result = getTableItems(caTable);
    if (closeShell) {
      caShell.close();
    }
        
    return result;
  }
	// ------------------------------------------------------------
	// Private
	// ------------------------------------------------------------
	/*
	 * Invokes QuickFix shell action
	 */
	private void invokeQuickFix() {
    ContextMenuHelper.clickContextMenu(editor, 
        IDELabel.Menu.QUICK_FIX);
	}

	/**
	 * Return list of table items. Requires activated proposal table
	 * @param table
	 * @return
	 */
	private List<String> getTableItems(SWTBotTable table) {
		int rows = table.rowCount();
		List<String> list = new ArrayList<String>();
		for (int i = 0; i < rows; i++) {
			list.add(table.cell(i, 0));
		}
		return list;
	}
	
	/**
	 * Returns Quick fix table from Quick Fix shell
	 * @param qfShell
	 * @return
	 */
	private SWTBotTable getQuickFixTable(SWTBotShell qfShell) {
		SWTBot qfBot = new SWTBot(qfShell.widget);
		return qfBot.table();		
	}

	/**
	 * Opens Quick Fix table
	 * @return Quick Fix bot shell
	 */
	private SWTBotShell openQuickFixList() {
		SWTBotShell[] shells1 = bot.shells();
		invokeQuickFix();
		SWTBotShell[] shells2 = bot.shells();
		SWTBotShell qfShell = getQuickFixShell(shells1, shells2);
		return qfShell;
	
	}

	/**
	 * Select table item from code completion table. It's workaround because
	 * SWTBotTable methods doesn't work property for this case
	 * 
	 * @param botTable
	 * @param index
	 */
	private void seletctQFTableItem(final SWTBotTable botTable, final int index) {
		UIThreadRunnable.asyncExec(new VoidResult() {
			public void run() {
				Table table = botTable.widget;
				table.setSelection(index);
				Event event = new Event();
				event.type = SWT.Selection;
				event.widget = table;
				event.item = table.getItem(index);
				table.notifyListeners(SWT.Selection, event);
				table.notifyListeners(SWT.DefaultSelection, event);
			}
		});
	}

	/**
	 * Return Quick Fix shell as new shell from two collections of shells
	 * 
	 * @param s1
	 * @param s2
	 * @return
	 */
	private SWTBotShell getQuickFixShell(SWTBotShell[] s1, SWTBotShell[] s2) {
		SWTBotShell ccShell = null;
		for (SWTBotShell bs2 : s2) {
			boolean found = false;
			for (SWTBotShell bs1 : s1) {
				if (bs2.widget.equals(bs1.widget)) {
					found = true;
					break;
				}
			}
			if (found == false) {
			  // New Shell has to contain table
			  try{
			    bs2.bot().table();
		      ccShell = bs2;
		      break;
			  }
			  catch (WidgetNotFoundException wnfe){
			    // do nothing
			  }
			}
		}
		return ccShell;
	}
	/**
	 * Check if quickFixItemLabel is present within Content Assist and 
	 * choose it when applyQuickFix is true
	 * @param quickFixItemLabel
	 * @param applyQuickFix
	 */
	public void checkQuickFix(String quickFixItemLabel,boolean applyQuickFix){
	  List<String> quickFixList = getQuickFixList();
	  
	  assertNotNull("Editor Quick Fix doesn't containt item with label: " + quickFixItemLabel +
	      ". It's null",
	    quickFixList);
	  
	  int itemIndex = quickFixList.indexOf(quickFixItemLabel);
	  
	  assertTrue("Editor Quick Fix doesn't containt item with label: " + quickFixItemLabel,
	    itemIndex > -1);
	  
	  if (applyQuickFix){
	    useQuickFix(itemIndex);
	  }
	  
	}
}
