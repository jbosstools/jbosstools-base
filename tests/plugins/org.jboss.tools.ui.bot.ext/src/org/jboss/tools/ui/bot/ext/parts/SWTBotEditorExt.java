 /*******************************************************************************
  * Copyright (c) 2007-2010 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/

package org.jboss.tools.ui.bot.ext.parts;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.allOf;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withText;

import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCTabItem;
import org.eclipse.ui.IEditorReference;
import org.jboss.tools.ui.bot.ext.Timing;

public class SWTBotEditorExt extends SWTBotEclipseEditor {

	
	public SWTBotEditorExt(IEditorReference editorReference, SWTWorkbenchBot bot)
			throws WidgetNotFoundException {
		super(editorReference, bot);

	}
	
	public ContentAssistBot contentAssist() {
		ContentAssistBot caBot = new ContentAssistBot(this);
		return caBot;
	}
	
	public void selectPage(int pageIndex){
	  try{
	    final CTabFolder tabFolder = this.findWidget(widgetOfType(CTabFolder.class));
	    final int tabFolderPageIndex = pageIndex;
	    bot.getDisplay().syncExec(new Runnable() {
        public void run() {
          if (tabFolder.getItemCount() > tabFolderPageIndex){
            tabFolder.setSelection(tabFolderPageIndex);
          }
        }
      });
	  } catch (WidgetNotFoundException wnfe){
	    // do nothing there is no tabfolder in editor
	  }
	}
	
	@SuppressWarnings("unchecked")
  public void selectPage(String pageLabel){
	    try{
	      final CTabFolder tabFolder = this.findWidget(widgetOfType(CTabFolder.class));
	      final CTabItem tabItem = ((CTabItem)this.bot.widget(allOf(widgetOfType(CTabItem.class),withText(pageLabel)),
	        tabFolder));
	      new SWTBotCTabItem(tabItem).activate();
	    } catch (WidgetNotFoundException wnfe){
	      // do nothing there is no tabfolder in editor
	    }
	}
	/**
	 * Deselect current selection and set cursor position to specified line and column
	 * @param line
	 * @param column
	 */
	public void deselectAndSetCursorPosition (int line, int column){
	  selectRange(0, 0, 0);
	  insertText(line, column, "");
	  save();
	  bot.sleep(Timing.time2S());
	}
}
	
