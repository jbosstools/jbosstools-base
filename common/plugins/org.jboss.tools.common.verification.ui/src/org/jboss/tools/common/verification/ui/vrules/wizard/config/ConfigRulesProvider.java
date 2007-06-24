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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.util.*;

public class ConfigRulesProvider extends LabelProvider implements ITreeContentProvider, ILabelProvider, IColorProvider {
	public Image IMAGE_ENABLED = EclipseResourceUtil.getImage("images/common/check.gif");
	public Image IMAGE_DISABLED = EclipseResourceUtil.getImage("images/common/uncheck.gif");
	protected RuleSetWrapper[] ruleSets = new RuleSetWrapper[0];
	
	public RuleSetWrapper[] getRuleSets() {
		return ruleSets;
	}

	public void setRuleSets(RuleSetWrapper[] ruleSets) {
		this.ruleSets = ruleSets;
	}

	public Object[] getChildren(Object parentElement) {
		return (parentElement instanceof RuleSetWrapper) ? ((RuleSetWrapper)parentElement).children : new Object[0];
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object element) {
		return (element instanceof RuleSetWrapper) && ((RuleSetWrapper)element).children.length > 0;
	}

	public Object[] getElements(Object inputElement) {
		return ruleSets;
	}

	public void dispose() {}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {}

	public Image getImage(Object element) {
		if(!(element instanceof ConfigItemWrapper)) return null;
		ConfigItemWrapper w = (ConfigItemWrapper)element;
		return (w.isSelected() && w.isEnabled()) ? IMAGE_ENABLED : IMAGE_DISABLED;
	}

	public Color getForeground(Object element) {
		if(!(element instanceof ConfigItemWrapper)) return null;
		ConfigItemWrapper w = (ConfigItemWrapper)element;
		return (w.isEnabled()) ? null : Display.getDefault().getSystemColor(SWT.COLOR_GRAY);
	}

	public Color getBackground(Object element) {
		return null;
	}
	
}
