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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;
import org.jboss.tools.common.model.util.*;

public class RuntimeRulesProvider extends LabelProvider implements ITreeContentProvider, ILabelProvider {
	public Image IMAGE_0 = EclipseResourceUtil.getImage("images/struts/pro/validator_constant.gif");
	public Image IMAGE_1 = EclipseResourceUtil.getImage("images/java/attr.gif");
	public Image IMAGE_2 = EclipseResourceUtil.getImage("images/struts/exception.gif");
	
	protected RuntimeRuleSetWrapper[] ruleSets = new RuntimeRuleSetWrapper[0];

	public RuntimeRuleSetWrapper[] getRuleSets() {
		return ruleSets;
	}

	public Object[] getChildren(Object parentElement) {
		return (parentElement instanceof RuntimeRuleSetWrapper) ? ((RuntimeRuleSetWrapper)parentElement).children : new Object[0];
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object element) {
		return (element instanceof RuntimeRuleSetWrapper) && ((RuntimeRuleSetWrapper)element).children.length > 0;
	}

	public Object[] getElements(Object inputElement) {
		return ruleSets;
	}

	public void dispose() {}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {}

	public Image getImage(Object element) {
		if(!(element instanceof RuntimeItemWrapper)) return null;
		RuntimeItemWrapper w = (RuntimeItemWrapper)element;
		int status = w.getStatus();
		return (status == 1) ? IMAGE_1 : (status == 2) ? IMAGE_2 : IMAGE_0;
	}
	
}
