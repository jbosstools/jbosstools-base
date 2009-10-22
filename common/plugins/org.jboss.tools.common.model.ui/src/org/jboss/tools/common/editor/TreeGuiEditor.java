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
package org.jboss.tools.common.editor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.*;
import org.eclipse.jface.viewers.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class TreeGuiEditor extends AbstractSectionEditor {
	private SashForm sash = null;
	private XModelObjectTreeViewComponent component;
	private XModelObject xmo = null;
//	private XModel model = null;
	private XFilteredTreeConstraint[] filters = new XFilteredTreeConstraint[0];
	
	
	public TreeGuiEditor() {}
	
	private void createGui() {
		sash = new SashForm(guiControl, SWT.HORIZONTAL);
		control = sash;
		control.setLayoutData(new GridData(GridData.FILL_BOTH));

		component = new XModelObjectTreeViewComponent(this);
		component.setModelObject(object);
		component.setCache(new XModelObjectCache(object));
		for (int i = 0; i < filters.length; i++) component.addFilter(filters[i]);
		component.createControl(sash);
	}

	protected void updateGui() {
		if(xmo == object) return;
		xmo = object;
		disposeGui();
//		if (object != null) {
//			model = object.getModel();
//		}
		createGui();
		fireGuiModified();
	}

	public TreeViewer getTreeViewer() {
		return component == null ? null : component.getViewer();
	}
	
	public ISelectionProvider getSelectionProvider() {
		return component == null ? null : component.getSelectionProvider();
	}
	
	public void fireEditorSelected() {
		if(getSelectionProvider() != null) component.selectionProvider.fireSelectionChanged();
	}
	
	public void addFilter(XFilteredTreeConstraint filter) {
		filters = new XFilteredTreeConstraint[]{filter};
	}
	
	protected void disposeGui() {
		if(component != null) component.dispose();
		component = null;
		if(sash != null && !sash.isDisposed()) sash.dispose();
		sash = null;
		super.disposeGui();
	}
}
