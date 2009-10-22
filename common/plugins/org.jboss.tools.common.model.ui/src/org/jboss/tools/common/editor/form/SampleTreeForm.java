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
package org.jboss.tools.common.editor.form;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import org.jboss.tools.common.editor.SelectionNotifier;
import org.jboss.tools.common.editor.XModelObjectTreeViewComponent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.actions.ActionFactory;

import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XFilteredTreeConstraint;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.XModelObjectCache;
import org.jboss.tools.common.model.ui.forms.ExpandableForm;
import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class SampleTreeForm extends ExpandableForm implements ISelectionChangedListener {
	
	private XModelObject xmo;
	private XModelObjectTreeViewComponent tree;
	private XFilteredTreeConstraint[] filters = new XFilteredTreeConstraint[0];
	private SelectionNotifier notifier;

	public SampleTreeForm(IEditorPart part) {
		tree = new XModelObjectTreeViewComponent(part);
		headingText = "Tree Form";
		setCollapsable(Boolean.FALSE.booleanValue());
		createActionMapping();
	}

	public void dispose() {
		super.dispose();
		if (tree!=null) tree.dispose();
		tree = null;
		if (actionMapping!=null) actionMapping.clear();
		actionMapping = null;
	}

	public void addFilter(XFilteredTreeConstraint filter) {
		filters = new XFilteredTreeConstraint[]{filter};
	}

	protected Control  createClientArea(Composite parent, IWidgetSettings settings) {
		BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, settings.getBorder("Table.Border")); //$NON-NLS-1$
		borderedControl.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		Control treeControl = tree.createControl(borderedControl, SWT.H_SCROLL | SWT.V_SCROLL);
		treeControl.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				tree.dispose();
			}
		});
		GridData gd = new GridData(GridData.FILL_BOTH);
		treeControl.setLayoutData(gd);
		
		if (notifier!=null) tree.getViewer().addSelectionChangedListener(notifier);
		return borderedControl;
	}
	
	public void initialize(Object model) {
		this.xmo = (XModelObject)model;
		this.model = xmo.getModel(); 
		setHeadingText(this.xmo.getAttributeValue("name")); //$NON-NLS-1$
		for (int i = 0; i < filters.length; i++) tree.addFilter(filters[i]);
		tree.setCache(new XModelObjectCache(xmo));
	}
	
	public void setSelectionNotifier(SelectionNotifier notifier) {
		ISelectionChangedListener oldListener = this.notifier;
		if ((tree!=null)&&(tree.getViewer()!=null)) {
			if (notifier!=null) {
				if (oldListener!=null) {
					tree.getViewer().removeSelectionChangedListener(oldListener);
				}
				tree.getViewer().addPostSelectionChangedListener(notifier);
			} else {
				tree.getViewer().removeSelectionChangedListener(notifier);
			}
		}
		this.notifier = notifier;
		notifier.addSelectionChangedListener(this);
	}	

	public void setHeadingText(String heading) {
		if(heading.equals(getHeadingText())) return;
		super.setHeadingText(heading);
//		if(header != null && !header.isDisposed()) {
//			header.setText(getHeadingText());
//		}
		if(section != null && !section.isDisposed()) {
			section.setText(getHeadingText());
		}
	}

	public void update() {
		super.update();
	}

	// ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)

	boolean selectionLock = false;
	public void selectionChanged(SelectionChangedEvent event) {
		if (tree == null || tree.getViewer() == null) return;
		if(selectionLock) return;
		selectionLock = true;
		try {
			Viewer viewer = tree.getViewer();
			ISelection oldSelection = viewer.getSelection();
			ISelection newSelection = event.getSelection();
			if (oldSelection.equals(newSelection)) return;
			viewer.setSelection(newSelection);
			if(viewer.getSelection().isEmpty() && !oldSelection.isEmpty()) {
				viewer.setSelection(oldSelection);
			}
		} finally {
			selectionLock = false;
		}
	}

	private HashMap<String,String> actionMapping = new HashMap<String,String>();
	private void createActionMapping() {
		actionMapping.put(ActionFactory.COPY.getId(), XAction.COPY);
		actionMapping.put(ActionFactory.CUT.getId(), XAction.CUT);
		actionMapping.put(ActionFactory.PASTE.getId(), XAction.PASTE);
		actionMapping.put(ActionFactory.DELETE.getId(), XAction.DELETE);
	}

	public boolean doGlobalAction(String actionId) {
		String actionPath = (String)actionMapping.get(actionId);
		if(actionPath == null) return false;
		Control c = tree.getViewer() == null ? null : tree.getViewer().getControl();
		if(c == null || !c.isFocusControl()) return false;
		ISelection selection = tree.getSelectionProvider().getSelection();
		if(selection == null || selection.isEmpty() || !(selection instanceof StructuredSelection)) return false;
		StructuredSelection ss = (StructuredSelection)selection;
		if(!(ss.getFirstElement() instanceof XModelObject)) return true;
		XModelObject object = (XModelObject)ss.getFirstElement();
		XModelObject[] os = null;
		if(ss.size() > 1) {
			os = new XModelObject[ss.size()];
			Iterator it = ss.iterator();
			for (int i = 0; i < os.length; i++) os[i] = (XModelObject)it.next(); 
		}
		XAction action = XActionInvoker.getAction(actionPath, object);
		if(action == null) return true;
		if(os == null) {
			if(!action.isEnabled(object)) return true;
			XActionInvoker.invoke(actionPath, object, new Properties());
		} else {
			if(!action.isEnabled(object, os)) return true;
			XActionInvoker.invoke(actionPath, object, os, new Properties());
		}
		return true;
	}

}
