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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeListener;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * 1. When button pressed, this control will be notify all listeners about it with:
 * PropertyChangeEvent(Object pressedButton, String actionName, Object null, Object Boolean.TRUE);
 * 
 * 2. When this control is disposeed, all listeners will be removed.
 */
public interface IButtonControl {

	// layout
	public void setLayout(Layout layout);
	public void setWidgetSettings(IWidgetSettings settings);

	// add / remove / clear
	public void addButton(String action, String label);
	public void removeButton(String action);
	public void clear();
	
	// create / dispose and enabled / disabled
	public Control createControl(Composite parent);
	public Control getControl();
	public void setEnabled(String action, boolean enabled);
	public void dispose();

	// listen the buttons
	public void addPropertyChangeListener(PropertyChangeListener listener);
	public void removePropertyChangeListener(PropertyChangeListener listener);
}
