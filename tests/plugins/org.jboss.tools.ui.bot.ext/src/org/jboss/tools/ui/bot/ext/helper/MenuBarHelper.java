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

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withMnemonic;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.hamcrest.Matcher;

/**
 * Helper to find menu within menu bar
 * @author Vladimir Pakan
 *
 */
public class MenuBarHelper {
  /**
   * Returns active shell menu bar menu with label menuLabel
   * @param menuLabel
   * @return
   */
  public static SWTBotMenu getMenu (final String menuLabel){
    final Shell activeShell = new SWTBot().activeShell().widget;
    final MenuItem menuItem = UIThreadRunnable.syncExec(new Result<MenuItem>() {
      @Override
      public MenuItem run() {
        Matcher<? extends Widget> matcher = withMnemonic(menuLabel);
        MenuItem[] items = activeShell.getMenuBar().getItems();
        int index = 0;
        while (index < items.length && !matcher.matches(items[index])){
          index++;
        }
        if (index < items.length){
          return items[index];  
        }
        else{
          throw new WidgetNotFoundException("Unable to find menu with label " + menuLabel +
            " within active shell menu bar.");
        }
        
      }
    });
    
    return new SWTBotMenu (menuItem);
    
  }    
}
