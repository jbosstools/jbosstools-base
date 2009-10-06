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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;

import org.jboss.tools.common.model.XModelObject;

public class PaletteViewPart extends ViewPart implements IPartListener, IPalettePageAdapter {
	
	public static final String VIEW_ID = "org.jboss.tools.common.model.ui.views.palette.PaletteView"; //$NON-NLS-1$

	PaletteCreator paletteCreator = new PaletteCreator(this);

	public PaletteViewPart() {}

	public IWorkbenchPage getPage() {
		return getSite().getPage();
	}

	public void createPartControl(Composite parent) {
		paletteCreator.createPartControlImpl(parent);
		paletteCreator.initActionBars();
	}

	public void setContentDescription(String description) {
		super.setContentDescription(description);
	}

	public void dispose() {
		super.dispose();
		paletteCreator.dispose();
		getSite().getPage().removePartListener(this);
	}

	public void setFocus() {
		paletteCreator.setFocus();
	}

	public void partActivated(IWorkbenchPart part) {
	    paletteCreator.partActivated(part);			
	}

	public void partBroughtToTop(IWorkbenchPart part) {
	}

	public void partClosed(IWorkbenchPart part) {
		paletteCreator.partClosed(part);
	}

	public void partDeactivated(IWorkbenchPart part) {}

	public void partOpened(IWorkbenchPart part) {}

	public void insertIntoEditor(XModelObject macro) {
		paletteCreator.insertIntoEditor(macro);
	}

	public boolean isEnabled() {
		return paletteCreator.isEnabled();
	}

	public IActionBars getActionBars() {
		return getViewSite() == null ? null : getViewSite().getActionBars();
	}

}
