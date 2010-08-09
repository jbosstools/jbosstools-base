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
package org.jboss.tools.ui.bot.ext.view;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarDropDownButton;
import org.hamcrest.core.IsAnything;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.View.GeneralConsole;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
/**
 * Manage Console View related tasks
 * @author Vlado Pakan
 *
 */
public class ConsoleView extends ViewBase {
  public static final int PROBLEMS_DESCRIPTION_COLUMN_INDEX = 0;
  public static final int PROBLEMS_RESOURCE_COLUMN_INDEX = 1;
  public static final int PROBLEMS_PATH_COLUMN_INDEX = 2;
  public static final int PROBLEMS_TYPE_COLUMN_INDEX = 4;
	Logger log = Logger.getLogger(ConsoleView.class);
	public ConsoleView() {
	  viewObject = GeneralConsole.LABEL;
	}

  /**
   * Returns actual console text
   * @return
   */
	public String getConsoleText(){
	  
	  show();
	  SWTBot viewBot = viewByTitle(viewObject.getName()).bot();
	  String consoleText = null;
	  try{
	    consoleText = viewBot.styledText().getText();
    }catch (WidgetNotFoundException wnfe){
      consoleText = null; 
    }
	  
	  return consoleText;
	  
	}
  /**
   * Clears console content	
   */
	public void clearConsole(){
	  
    show();
    SWTBot viewBot = viewByTitle(viewObject.getName()).bot();
    try{
      SWTBotButton clearConsole = viewBot.buttonWithTooltip(IDELabel.ConsoleView.BUTTON_CLEAR_CONSOLE_TOOLTIP).click();
      clearConsole.click();
    }catch (WidgetNotFoundException wnfe){
      // Do nothing Clear Console button is not available 
    }
	}
	/**
	 * switches content displayed in cosole (see 'Display Selected Console' toolbar button)
	 * if content cannot be switched (e.g. only one console is displayed in view or console name does not match given
	 * String param) return false
	 * @param containedInonsoleName String contained in name of console to switch on
	 */
	public boolean switchConsole(String containedInonsoleName) {
		SWTBotView consoleView = show();
		SWTBotToolbarDropDownButton button = consoleView.toolbarDropDownButton(IDELabel.ConsoleView.BUTTON_DISPLAY_SELECTED_CONSOLE_TOOLTIP);
		if (button.isEnabled()) {
			for (SWTBotMenu menu : button.menuItems(new IsAnything<MenuItem>())) 
			{
				if (menu.getText().contains(containedInonsoleName)) {
					log.info("Switching consoleView to display '"+menu.getText()+"'. console");
					menu.click();
					return true;
				}
			}
		}
		return false;
	}
	
	public String getConsoleText (long sleepTime , long timeOut , boolean quitWhenNoChange){
	  
    long estimatedTime = 0;
    SWTBot viewBot = viewByTitle(viewObject.getName()).bot();
    String prevConsoleText = getConsoleText();
    String consoleText = prevConsoleText;
    log.info("Waiting for console text with TimeOut: " + timeOut);
    while ((estimatedTime < timeOut)
      && (!quitWhenNoChange 
        || estimatedTime == 0  
        || prevConsoleText == null
        || prevConsoleText.length() == 0
        || !prevConsoleText.equals(consoleText))){
      prevConsoleText = consoleText;
      viewBot.sleep(sleepTime);
      estimatedTime += sleepTime;
      consoleText = getConsoleText();
    }
    log.info("Waiting for console text finished");
    
    return consoleText;
    
	}
	
}
