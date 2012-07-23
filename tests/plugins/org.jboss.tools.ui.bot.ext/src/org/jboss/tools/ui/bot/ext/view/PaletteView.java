/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.view;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.gen.ActionItem;
/**
 * Represents JBoss Tools Palette View
 * @author vpakan@redhat.com
 *
 */
public class PaletteView extends ViewBase {
	Logger log = Logger.getLogger(PaletteView.class);
	public PaletteView() {
		viewObject = ActionItem.View.JBossToolsWebJBossToolsPalette.LABEL;		
	}

}
