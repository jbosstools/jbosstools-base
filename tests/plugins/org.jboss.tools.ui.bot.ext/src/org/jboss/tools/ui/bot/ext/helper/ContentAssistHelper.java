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

import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.List;
import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
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

    SWTJBTExt.selectTextInSourcePane(bot,
        editorTitle, textToSelect, selectionOffset, selectionLength,
        textToSelectIndex);

    bot.sleep(Timing.time1S());

    SWTBotEditorExt editor = SWTTestExt.bot.swtBotEditorExtByTitle(editorTitle);
    ContentAssistBot contentAssist = editor.contentAssist();
    List<String> currentProposalList = contentAssist.getProposalList();
    assertTrue("Code Assist menu has incorrect menu items.\n" +
        "Expected Proposal Menu Labels vs. Current Proposal Menu Labels :\n" + 
        ContentAssistHelper.getListsDiffFormatted(expectedProposalList,currentProposalList),
      expectedProposalList.equals(currentProposalList));

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

    return ContentAssistHelper.checkContentAssistContent(bot, 
        editorTitle, 
        textToSelect, 
        selectionOffset, 
        selectionLength,
        0,
        expectedProposalList);

  }
  /**
   * Get formatted difference between list0 and list1
   * @param list0
   * @param list1
   * @return
   */
  private static String getListsDiffFormatted (List<String> list0 , List<String> list1){
    StringBuffer sb = new StringBuffer("");
    if (list0 == null && list1 == null){
      sb.append("<null> == <null>");
    }else if (list0 == null){
      sb.append("<null> != <not null>");
    }else if (list1 == null){
      sb.append("<null> != <not null>");
    } else{
      Iterator<String> iterator0 = list0.iterator();
      Iterator<String> iterator1 = list1.iterator();
      boolean continueIteration = iterator0.hasNext() || iterator1.hasNext();
      while (continueIteration){
        String item0 = "<null>";
        String item1 = "<null>";
        sb.append("  ");
        if (iterator0.hasNext()){
          item0 = iterator0.next();
        }
        if (iterator1.hasNext()){
          item1 = iterator1.next();
        }
        sb.append("  ");
        sb.append(item0);
        sb.append(item0.equals(item1) ? " == " : " != ");
        sb.append(item1);
        continueIteration = iterator0.hasNext() || iterator1.hasNext();
        if (continueIteration){
          sb.append("\n");
        }
      }
    }
    System.out.println(sb.toString());
    for (String item : list1){
      System.out.println("result.add(\"" + item.replaceAll("\"", "\\\"") + "\");");
    }

    return sb.toString();
  }
  
} 
