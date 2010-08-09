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

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.allOf;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withTooltip;

import java.util.List;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.jboss.tools.ui.bot.ext.widgets.SWTBotImageHyperlink;
/**
 * Helper to find widgets
 * @author Vladimir Pakan
 *
 */
public class ImageHyperlinkHelper {
  /**
   * Returns {@link SWTBotImageHyperlink} with specified tooltip  
   * @param bot
   * @param tooltiptext
   * @return
   * @throws WidgetNotFoundException
   */
  @SuppressWarnings("unchecked")
  public static SWTBotImageHyperlink imageHyperlinkWithTooltip(SWTBot bot, String tooltipText) throws WidgetNotFoundException{
    List<?> imageHyperlinks = bot.widgets(allOf(
      widgetOfType(ImageHyperlink.class),
      withTooltip(tooltipText)));
    if (imageHyperlinks != null && imageHyperlinks.size() > 0){
      return new SWTBotImageHyperlink((ImageHyperlink)imageHyperlinks.get(0));
    }
    else{
      throw new WidgetNotFoundException("Unable to find ImageHyperlink widget " +
        "with tooltip: " + tooltipText);
    }
  }
  /**
   * Displays all ImageHyperlinks to console contained in specified bot
   * @param bot
   * @throws WidgetNotFoundException
   */
  public static void displayAllImageHyperlinks(SWTBot bot)
      throws WidgetNotFoundException {
    List<?> imageHyperlinks = bot.widgets(widgetOfType(ImageHyperlink.class));
    for (Object o : imageHyperlinks) {
      ImageHyperlink imageHyperlink = (ImageHyperlink) o;
      SWTBotImageHyperlink link = new SWTBotImageHyperlink(imageHyperlink);
      System.out.println(link.toString());
    }
  }
      
}
