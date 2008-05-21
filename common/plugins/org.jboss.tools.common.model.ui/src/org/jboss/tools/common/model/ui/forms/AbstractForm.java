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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.IMemento;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class AbstractForm implements IForm {
	
	// Decoration properties
	protected Color headingBackground;
	protected Color headingForeground;
	protected Image headingImage;
	protected String headingText = "%No text%";
	private Object layoutData;
	private Layout layout;
	private boolean enabled = Boolean.TRUE.booleanValue();
	protected boolean headingVisible;
	private IFormContainer parent;
	
	public AbstractForm() {}
	
	// GUI	
	public abstract Control createControl(Composite parent, IWidgetSettings factory);
	public abstract Control getControl();
	public abstract void setFocus();
	public abstract void dispose();
	
	// Model data manipulation
	public abstract void initialize(Object model);
	public abstract void commitChanges(boolean onSave);
	public abstract boolean doGlobalAction(String actionId);
	public abstract void expandTo(Object object);
	public abstract void update();
	public abstract void store(IMemento memento);
	public abstract void load(IMemento memento);

	// Decoration properties getters and setters
	public Color getHeadingBackground() {
		return headingBackground;
	}

	public void setHeadingBackground(Color newHeadingBackground) {
		headingBackground = newHeadingBackground;
	}

	public Color getHeadingForeground() {
		return headingForeground;
	}
	public void setHeadingForeground(Color newHeadingForeground) {
		headingForeground = newHeadingForeground;
	}

	public Image getHeadingImage() {
		return headingImage;
	}
	public void setHeadingImage(Image newHeadingImage) {
		headingImage = newHeadingImage;
	}

	public String getHeadingText() {
		return headingText;
	}
	public void setHeadingText(String heading) {
		headingText = heading;
	}

	public boolean isHeadingVisible() {
		return headingVisible;
	}
	public void setHeadingVisible(boolean newHeadingVisible) {
		headingVisible = newHeadingVisible;
	}

    public IFormContainer getParent() {
        return parent;
    }

    public void setParent(IFormContainer container) {
        parent = container;
    }

	public void setLayoutData(Object layoutData) {
		this.layoutData = layoutData;
	}
	
	public Object getLayoutData() {
		if (layoutData==null) {
			layoutData = new GridData(GridData.FILL_HORIZONTAL);
		}
		return layoutData;
	}
	
	public void setLayout(Layout layout) {
		this.layout = layout;
	}
	public Layout getLayout() {
		if (layout==null) {
			GridLayout layout = new GridLayout();
			layout.horizontalSpacing = 0;
			layout.verticalSpacing = 0;
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			this.layout = layout;
		}
		return layout;
	}
	
	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

}