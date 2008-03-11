/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.views.palette;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.jboss.tools.common.model.ui.views.palette.model.*;
import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.text.AbstractHoverInformationControlManager;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;

public class PaletteDescriptionManager extends AbstractHoverInformationControlManager {		

	private ToolBar fToolBar = null;
	private boolean fWindowsFlag;
	
	public PaletteDescriptionManager() {
		super(new DescriptionControlCreator());
		setSizeConstraints(60, 30, false, false);
		fWindowsFlag = true;
		String osName = System.getProperty("os.name");
		fWindowsFlag = osName != null && osName.toUpperCase().indexOf("WINDOWS") != -1;
	}
	
	protected void computeInformation() {
		if (fToolBar != null) {
			ToolItem item = fToolBar.getItem(0);
			Rectangle barArea = fToolBar.getBounds();
			Rectangle itemArea = item.getBounds();
			Rectangle area = new Rectangle(barArea.x + itemArea.x, barArea.y + itemArea.y, itemArea.width, itemArea.height );
			IPaletteNode node = (IPaletteNode)item.getData();
			String description = node.getDescription();
			setInformation(description, area);
		} else {
			setInformation(null, null);
		}
	}
	
	static private class DescriptionControlCreator implements IInformationControlCreator {
		public IInformationControl createInformationControl(Shell parent) {
			return new DefaultInformationControl(parent, SWT.NONE, new HTMLTextPresenter(true));
		}
	}

}
