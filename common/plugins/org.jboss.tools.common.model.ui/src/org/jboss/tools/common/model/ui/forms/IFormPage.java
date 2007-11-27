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

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public interface IFormPage {
	public boolean becomesInvisible(IFormPage newPage);
	public void becomesVisible(IFormPage previousPage);
	
	public Control createControl(Composite parent);
	public Control getControl();
	public void dispose();
	
	public ISelectionProvider getSelectionProvider();
	
	public String getLabel();
	public void setLabel(String label);
	public String getTitle();
	public void setTitle(String title);
	
	public void initialize(Object model);
	public void commitChanges(boolean onSave);
	public void expandTo(Object object);
	public void update();
	
}
