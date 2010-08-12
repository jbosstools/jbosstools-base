/*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/

package org.jboss.tools.ui.bot.ext.helper;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.StringResult;

/**
 * Helper to find widgets
 * @author Vladimir Pakan
 *
 */
public class WidgetFinderHelper {
  /**
   * Returns Browser contained in Editor with specified Title
   * @param bot
   * @param editorTitle
   * @return
   */
  public static Browser browserInEditor(SWTWorkbenchBot bot , String editorTitle){
    return browserInEditor(bot.editorByTitle(editorTitle));
  }
  /**
   * Returns Browser contained in specified Editor
   * @param editor
   * @return
   */
  public static Browser browserInEditor(SWTBotEditor editor){
    return ((Browser)editor.bot().widgets(widgetOfType(Browser.class)).get(0));
  }
  
  public static final String browserInEditorText(SWTWorkbenchBot bot , String editorTitle , boolean closeEditor){
    
    String browserText = null;
    
    try{
      
      SWTBotEditor editor = bot.editorByTitle(editorTitle);
      final Browser browser = browserInEditor(editor);
      
      browserText = UIThreadRunnable
        .syncExec(new StringResult() {
          public String run() {
            return browser.getText();
        }
      });
      
      if (closeEditor){
        editor.close();    
      }

    } catch (WidgetNotFoundException wnfe){
      // do nothing returns null
    }
    
    return browserText;
    
  }
    
}
