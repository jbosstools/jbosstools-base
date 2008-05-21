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

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public interface IForm {
	public Control createControl(Composite parent, IWidgetSettings settings);
	public Control getControl();
	public void setFocus();
	public boolean doGlobalAction(String actionId);
	public boolean isEnabled();
	public void setEnabled(boolean enabled);
	public void dispose();

	public void initialize(Object model);
	public void commitChanges(boolean onSave);
	public void expandTo(Object object);
	public void update();
	
	public void store(IMemento memento);
	public void load(IMemento memento);

	public Color getHeadingBackground();
	public Color getHeadingForeground();
	public Image getHeadingImage();
	public String getHeadingText();
	public boolean isHeadingVisible();
	public void setHeadingBackground(Color newHeadingBackground);
	public void setHeadingForeground(Color newHeadingForeground);
	public void setHeadingImage(Image headingImage);
	public void setHeadingVisible(boolean newHeadingVisible);
	public void setHeadingText(String heading);
	public void setParent(IFormContainer container);
	public IFormContainer getParent();
}
