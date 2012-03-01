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

import java.awt.event.KeyEvent;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTJBTExt;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.Timing;

/**
 * Helper for Open On functionality testing
 * @author Vladimir Pakan
 *
 */
public class OpenOnHelper {
  protected static final Logger log = Logger.getLogger(OpenOnHelper.class);
  /**
   * Applies Open On (F3) on textToSelect within editor with editorTitle
   * and checks if expectedOpenedFileName was opened 
   * @param editorTitle
   * @param textToSelect
   * @param selectionOffset
   * @param selectionLength
   * @param textToSelectIndex
   * @param expectedOpenedFileName
   */
  public static SWTBotEditor checkOpenOnFileIsOpened(SWTBotExt bot,
      String editorTitle, String textToSelect, int selectionOffset,
      int selectionLength, int textToSelectIndex, String expectedOpenedFileName) {

    SWTBotEditor openedEditor = null;

    SWTBotEclipseEditor sourceEditor = SWTJBTExt.selectTextInSourcePane(bot,
        editorTitle, textToSelect, selectionOffset, selectionLength,
        textToSelectIndex);

    bot.sleep(Timing.time1S());

    sourceEditor.setFocus();

    bot.sleep(Timing.time1S());

    KeyboardHelper.typeKeyCodeUsingAWT(KeyEvent.VK_F3);

    bot.sleep(Timing.time1S());
    new SWTUtilExt(bot).waitForNonIgnoredJobs();

    openedEditor = bot.activeEditor();

    assertTrue("Opened file has to have title " + expectedOpenedFileName
        + " but has " + openedEditor.getTitle(), openedEditor.getTitle()
        .equalsIgnoreCase(expectedOpenedFileName));

    return openedEditor;

  }
  
} 
