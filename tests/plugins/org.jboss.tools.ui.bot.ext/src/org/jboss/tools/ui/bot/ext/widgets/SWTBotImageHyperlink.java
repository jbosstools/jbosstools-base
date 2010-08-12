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

package org.jboss.tools.ui.bot.ext.widgets;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.jboss.tools.ui.bot.ext.parts.SWTBotHyperlinkExt;

/**
 * SWTBot for ImageHyperlink widget
 * @author Vladimir Pakan
 *
 */
public class SWTBotImageHyperlink extends SWTBotHyperlinkExt{
  /**
   * Constructs a new instance with the given ImageHyperlink.
   * 
   * @param imageHyperlink the ImageHyperlink.
   * @throws WidgetNotFoundException if the imageHyperlink is <code>null</code> or widget has been disposed.
   */
  public SWTBotImageHyperlink(ImageHyperlink w) throws WidgetNotFoundException {
    super(w);
  }    
  /**
   * Display ImageHyperlink
   */
  public String toString(){
    String text = getText();
    String tooltip = getToolTipText();
    return "ImageHyperlink Text: " + (text != null ? text : "<null>")
      + " Tooltip: " + (tooltip != null ? tooltip : "<null>");
  }
}
