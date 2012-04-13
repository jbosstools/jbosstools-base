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

package org.jboss.tools.ui.bot.ext.helper;

import static org.junit.Assert.assertTrue;

import java.util.List;
import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.jboss.tools.ui.bot.ext.FormatUtils;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTJBTExt;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.Timing;
import org.jboss.tools.ui.bot.ext.parts.QuickFixBot;
import org.jboss.tools.ui.bot.ext.parts.SWTBotEditorExt;

/**
 * Helper for Quick Fix functionality testing
 * @author Vladimir Pakan
 *
 */
public class QuickFixHelper {
  protected static final Logger log = Logger.getLogger(QuickFixHelper.class);
  /**
   * Checks Quick Fix content on specified position within editor with editorTitle
   * and checks if expectedQuickFixList is equal to current Quick Fix List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedQuickFixsList
   */
  public static SWTBotEditor checkQuickFixContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, int textToSelectIndex, List<String> expectedQuickFixsList) {

    SWTJBTExt.selectTextInSourcePane(bot,
        editorTitle, textToSelect, selectionOffset, selectionLength,
        textToSelectIndex);

    bot.sleep(Timing.time1S());

    SWTBotEditorExt editor = SWTTestExt.bot.swtBotEditorExtByTitle(editorTitle);
    QuickFixBot quickFix = editor.quickFix();
    List<String> currentQuickFixsList = quickFix.getQuickFixList();
    assertTrue("Quick Fix menu has incorrect menu items.\n" +
        "Expected Qucik Fix Menu Labels vs. Current Quick Fix Menu Labels :\n" + 
        FormatUtils.getListsDiffFormatted(expectedQuickFixsList,currentQuickFixsList),
        expectedQuickFixsList.equals(currentQuickFixsList));

    return editor;

  }
  
  /**
   * Checks Quick Fix content on specified position within editor with editorTitle
   * and checks if expectedQuickFixsList is equal to current Quick Fix List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedQuickFixsList
   */
  public static SWTBotEditor checkQuickFixContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, List<String> expectedQuickFixsList) {

    return QuickFixHelper.checkQuickFixContent(bot, 
        editorTitle, 
        textToSelect, 
        selectionOffset, 
        selectionLength,
        0,
        expectedQuickFixsList);

  }
  
} 
