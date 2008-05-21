/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.gef.editor.xpl;

import org.eclipse.gef.palette.*;
import org.eclipse.gef.ui.palette.PaletteCustomizer;
import org.eclipse.gef.ui.palette.customize.DefaultEntryPage;
import org.eclipse.gef.ui.palette.customize.DrawerEntryPage;
import org.eclipse.gef.ui.palette.customize.EntryPage;

/**
 * 
 * 
 * @author Pratik Shah
 */
public class DefaultPaletteCustomizer extends PaletteCustomizer {

	protected static final String ERROR_MESSAGE = "Error";

	/**
	 * @see org.eclipse.gef.ui.palette.PaletteCustomizer#getPropertiesPage(PaletteEntry)
	 */
	public EntryPage getPropertiesPage(PaletteEntry entry) {
		if (entry.getType().equals(PaletteDrawer.PALETTE_TYPE_DRAWER)) {
			return new ToolsDrawerEntryPage();
		}
		return new ToolsEntryPage();
	}

	/**
	 * @see org.eclipse.gef.ui.palette.PaletteCustomizer#revertToSaved()
	 */
	public void revertToSaved() {
	}

	/**
	 * @see org.eclipse.gef.ui.palette.PaletteCustomizer#dialogClosed(PaletteEntry)
	 */
	public void save() {
	}

	private class ToolsEntryPage extends DefaultEntryPage {
		protected void handleNameChanged(String text) {
			if (text.indexOf('*') >= 0) {
				getPageContainer().showProblem(ERROR_MESSAGE);
			} else {
				super.handleNameChanged(text);
				getPageContainer().clearProblem();
			}
		}
	}

	private class ToolsDrawerEntryPage extends DrawerEntryPage {
		protected void handleNameChanged(String text) {
			if (text.indexOf('*') >= 0) {
				getPageContainer().showProblem(ERROR_MESSAGE);
			} else {
				super.handleNameChanged(text);
				getPageContainer().clearProblem();
			}
		}
	}

}
