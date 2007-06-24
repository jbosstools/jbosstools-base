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
package org.jboss.tools.common.model.ui.forms;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public interface IFormWorkbook {
	void addFormSelectionListener(IFormSelectionListener listener);
	public void addPage(IFormPage page);
	public void createControl(Composite parent);
	Control getControl();
	public IFormPage getCurrentPage();
	boolean isFirstPageSelected();
	void removeFormSelectionListener(IFormSelectionListener listener);
	public void removePage(IFormPage page);
	public void selectPage(final IFormPage page, boolean setFocus);
	void setFirstPageSelected(boolean selected);
}
