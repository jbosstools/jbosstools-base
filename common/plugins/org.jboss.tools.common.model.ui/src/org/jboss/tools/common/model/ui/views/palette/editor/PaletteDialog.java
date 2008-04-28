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
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.meta.action.XActionItem;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.meta.action.XActionItem.Acceptor;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.navigator.FilteredTreeContentProvider;
import org.jboss.tools.common.model.ui.navigator.NavigatorLabelProvider;
import org.jboss.tools.common.model.ui.navigator.TreeViewerMenuInvoker;
import org.jboss.tools.common.model.ui.navigator.TreeViewerModelListenerImpl;
import org.jboss.tools.common.model.ui.objecteditor.XModelObjectEditor;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTSync;

public class PaletteDialog extends Dialog {
	protected TreeViewer treeViewer = null;
	protected XModelObjectEditor objectEditor = new XModelObjectEditor();
	protected FilteredTreeContentProvider contentProvider = new FilteredTreeContentProvider();
	protected TreeViewerModelListenerImpl listener = new TreeViewerModelListenerImpl();
	protected XModelTreeListenerSWTSync syncListener = new XModelTreeListenerSWTSync(listener);
	protected XModelTreeListener listener2 = new ObjectListener();
	protected XModelTreeListenerSWTSync syncListener2 = new XModelTreeListenerSWTSync(listener2);

	TreeViewerMenuInvoker menu = new TreeViewerMenuInvoker() {
		protected XActionList getActionList(XModelObject o) {
			XActionList l = super.getActionList(o);
			if(l != null) {
				// Filter out 'Properties' item which is redundant in this editor.
				l = (XActionList)l.copy(new Acceptor() {
			        public boolean accepts(XActionItem item) {
			        	if(item.getName().startsWith("Propert")) {
			        		return false;
			        	}
			        	return true;
			        }				
				});
			}
			return l;
		}		
	};

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
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, ModelUIPlugin.ID_PLUGIN + ".palette_editor");
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
			HelpUtil.helpEclipse(PreferenceModelUtilities.getPreferenceModel(), "PaletteEditor");
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
