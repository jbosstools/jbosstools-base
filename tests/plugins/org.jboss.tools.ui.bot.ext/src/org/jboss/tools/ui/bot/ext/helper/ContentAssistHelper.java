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
import org.jboss.tools.ui.bot.ext.parts.ContentAssistBot;
import org.jboss.tools.ui.bot.ext.parts.SWTBotEditorExt;

/**
 * Helper for Content Assist functionality testing
 * @author Vladimir Pakan
 *
 */
public class ContentAssistHelper {
  protected static final Logger log = Logger.getLogger(ContentAssistHelper.class);
  /**
   * Checks Content Assist content on specified position within editor with editorTitle
   * and checks if expectedProposalList is equal to current Proposal List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedProposalList
   */
  public static SWTBotEditor checkContentAssistContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, int textToSelectIndex, List<String> expectedProposalList) {

    return checkContentAssistContent(bot, editorTitle, textToSelect, selectionOffset, 
    		selectionLength, textToSelectIndex, expectedProposalList, true);

  }
  
  /**
   * Checks Content Assist content on specified position within editor with editorTitle
   * and checks if expectedProposalList is equal to current Proposal List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedProposalList
   * @param mustEquals
   */
  public static SWTBotEditor checkContentAssistContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, int textToSelectIndex, List<String> expectedProposalList,
      boolean mustEquals) {

    SWTJBTExt.selectTextInSourcePane(bot,
        editorTitle, textToSelect, selectionOffset, selectionLength,
        textToSelectIndex);

    bot.sleep(Timing.time1S());

    SWTBotEditorExt editor = SWTTestExt.bot.swtBotEditorExtByTitle(editorTitle);
    ContentAssistBot contentAssist = editor.contentAssist();
    List<String> currentProposalList = contentAssist.getProposalList();
    assertTrue("Code Assist menu has incorrect menu items.\n" +
        "Expected Proposal Menu Labels vs. Current Proposal Menu Labels :\n" + 
        FormatUtils.getListsDiffFormatted(expectedProposalList,currentProposalList),
      mustEquals?expectedProposalList.equals(currentProposalList):
    	  currentProposalList.containsAll(expectedProposalList));

    return editor;

  }
  
  /**
   * Checks Content Assist content on specified position within editor with editorTitle
   * and checks if expectedProposalList is equal to current Proposal List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedProposalList
   */
  public static SWTBotEditor checkContentAssistContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, List<String> expectedProposalList) {

    return checkContentAssistContent(bot, editorTitle, textToSelect, 
    		selectionOffset, selectionLength, expectedProposalList, true);

  }
  
  /**
   * Checks Content Assist content on specified position within editor with editorTitle
   * and checks if expectedProposalList is equal to current Proposal List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedProposalList
   * @param mustEquals
   */
  public static SWTBotEditor checkContentAssistContent(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, List<String> expectedProposalList, boolean mustEquals) {

    return ContentAssistHelper.checkContentAssistContent(bot, 
        editorTitle, 
        textToSelect, 
        selectionOffset, 
        selectionLength,
        0,
        expectedProposalList,
        mustEquals);

  }
  
  /**
   * Checks Content Assist auto proposal. It's case when there is only one
   * content assist item and that item is automatically inserted into editor
   * and checks if expectedProposalList is equal to current Proposal List 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedInsertedText
   */
  public static SWTBotEditor checkContentAssistAutoProposal(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, int textToSelectIndex, String expectedInsertedText) {

    SWTJBTExt.selectTextInSourcePane(bot,
        editorTitle, textToSelect, selectionOffset, selectionLength,
        textToSelectIndex);

    bot.sleep(Timing.time1S());

    SWTBotEditorExt editor = SWTTestExt.bot.swtBotEditorExtByTitle(editorTitle);
    String editorLineBeforeInsert = editor.getTextOnCurrentLine();
    int xPos = editor.cursorPosition().column;
    String expectedEditorLineAfterInsert = editorLineBeforeInsert.substring(0,xPos) +
        expectedInsertedText +
        editorLineBeforeInsert.substring(xPos);
    ContentAssistBot contentAssist = editor.contentAssist();
    contentAssist.invokeContentAssist();
    String editorLineAfterInsert = editor.getTextOnCurrentLine();
    assertTrue("Text on current line should be:\n" +
        "but is :\n" + editorLineAfterInsert
        , editorLineAfterInsert.equals(expectedEditorLineAfterInsert));

    return editor;

  }
} 
