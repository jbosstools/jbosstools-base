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
package org.jboss.tools.common.model.ui.wizards.special;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;

public interface ISpecialWizardStep {
//	public void addPropertyChangeListener(PropertyChangeListener l);
	public void setSupport(SpecialWizardSupport support, int i);
	public Control createControl(Composite parent);
	public void update();
	public void save();
	public void clear();
	public boolean addIcon();
	public void dispose();
	public void validate();
	public boolean isDataChanged();
	
	// size
	public Point getMinimumSize();
	public Point getMaximumSize();	
}
