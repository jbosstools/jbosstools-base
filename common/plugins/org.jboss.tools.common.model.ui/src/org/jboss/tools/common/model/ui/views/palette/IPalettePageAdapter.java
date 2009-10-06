/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.views.palette;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPage;
import org.jboss.tools.common.model.XModelObject;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public interface IPalettePageAdapter {
	public IActionBars getActionBars();
	public void insertIntoEditor(XModelObject macro);
	public boolean isEnabled();
	public void setContentDescription(String description);
	public IWorkbenchPage getPage();
}
