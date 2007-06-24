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
package org.jboss.tools.common.model.ui.views.palette.editor;

import org.jboss.tools.common.model.util.XModelTreeListenerSWTSync;
import org.jboss.tools.common.model.ui.navigator.*;
import org.jboss.tools.common.model.ui.objecteditor.XModelObjectEditor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;

public class PaletteDialog extends Dialog {
	protected TreeViewer treeViewer = null;
	protected XModelObjectEditor objectEditor = new XModelObjectEditor();
	protected FilteredTreeContentProvider contentProvider = new FilteredTreeContentProvider();
	protected TreeViewerModelListenerImpl listener = new TreeViewerModelListenerImpl();
	protected XModelTreeListenerSWTSync syncListener = new XModelTreeListenerSWTSync(listener);
	protected XModelTreeListener listener2 = new ObjectListener();
	protected XModelTreeListenerSWTSync syncListener2 = new XModelTreeListenerSWTSync(listener2);
	TreeViewerMenuInvoker menu = new TreeViewerMenuInvoker();
	PaletteDialogState state = new PaletteDialogState(this);
	SL sl = new SL();
	SashForm sash;
	Composite control;
	
	public void dispose() {
		if (treeViewer!=null && treeViewer.getTree()!=null && !treeViewer.getTree().isDisposed()) {
			treeViewer.getTree().removeSelectionListener(sl);
		}
		sl = null;
		treeViewer = null;
		if (objectEditor!=null) objectEditor.dispose();
		objectEditor = null;
		if (syncListener!=null) syncListener.dispose();
		syncListener = null;
		if (syncListener2 != null) syncListener2.dispose();
		syncListener2 = null;

	}
	
	public PaletteDialog(Shell shell) {
		super(shell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		menu.setOnKeyRelease(true);
	}
	
	public void create() {
		super.create();
		getShell().setText("Palette Editor");
		state.loadState();
	}

	protected Control createContents(Composite parent) {
		Control c = super.createContents(parent);
		createEditorComponents((Composite)getDialogArea());
		control = (Composite)c;
		return c;
	}
		
	protected void createEditorComponents(Composite parent) {
		SashForm panel = new SashForm(parent, SWT.NONE);
		sash = panel;
		panel.setLayoutData(new GridData(GridData.FILL_BOTH));
		treeViewer = new TreeViewer(panel);
		
		contentProvider.setModel(PreferenceModelUtilities.getPreferenceModel());
		contentProvider.setViewer(treeViewer);
		contentProvider.setFilteredTreeName("PaletteTree");
		treeViewer.setContentProvider(contentProvider);
		treeViewer.setLabelProvider(new NavigatorLabelProvider());
		treeViewer.setInput(contentProvider);
		treeViewer.refresh();

		menu.setViewer(treeViewer);
		treeViewer.getTree().addMouseListener(menu);
		treeViewer.getTree().addKeyListener(menu);
		treeViewer.getTree().addSelectionListener(sl);	
		treeViewer.getTree().setVisible(true);
		treeViewer.setAutoExpandLevel(2);	
		objectEditor.createControl(panel);
		activate();
		treeViewer.setAutoExpandLevel(2);
	}
	
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,	false);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL,	false);
///		createButton(parent, IDialogConstants.HELP_ID, IDialogConstants.HELP_LABEL,	false);
	}
	
	class SL extends SelectionAdapter {
		public void widgetSelected(SelectionEvent e) {
			objectEditor.setModelObject(menu.getSelectedModelObject());
		}
	}
		
	protected void okPressed() {
		objectEditor.stopEditing();
		super.okPressed();
	}
	
	public void activate() {
		listener.setViewer(treeViewer);
		PreferenceModelUtilities.getPreferenceModel().addModelTreeListener(syncListener);
		PreferenceModelUtilities.getPreferenceModel().addModelTreeListener(syncListener2);
	}
	
	public boolean close() {
		state.saveState();
		boolean b = super.close();
		if(b) {
			PreferenceModelUtilities.getPreferenceModel().removeModelTreeListener(syncListener);
			PreferenceModelUtilities.getPreferenceModel().removeModelTreeListener(syncListener2);
		}
		dispose();
		return b;
	}
	
	protected void buttonPressed(int buttonId) {
		if(buttonId == IDialogConstants.HELP_ID) {
			try {
				HelpUtil.helpEclipse(PreferenceModelUtilities.getPreferenceModel(), "PaletteEditor");
			} catch (Exception e) {}
		} else {
			super.buttonPressed(buttonId);
		}
	}
	
	class ObjectListener implements XModelTreeListener {

		public void nodeChanged(XModelTreeEvent event) {
			if(event.getModelObject() == menu.getSelectedModelObject()) {
				objectEditor.update();
			}
		}

		public void structureChanged(XModelTreeEvent event) {}		
	}
}
