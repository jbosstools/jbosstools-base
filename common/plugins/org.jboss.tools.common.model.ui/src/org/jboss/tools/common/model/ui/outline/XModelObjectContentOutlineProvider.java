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
package org.jboss.tools.common.model.ui.outline;

import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.contentoutline.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.ControlDragDrop;
import org.jboss.tools.common.model.ui.navigator.*;
import org.jboss.tools.common.model.ui.select.XModelObjectSelectionProvider;
import org.eclipse.jface.viewers.*;

public class XModelObjectContentOutlineProvider extends ContentOutlinePage {
	XModel model;
	XModelObjectCache cache;
	OutlineContentProvider content = new OutlineContentProvider();
	TreeViewerMenuInvoker menu = new OutlineMenuInvoker();
	TreeViewerModelListenerImpl listener = new TreeViewerModelListenerImpl(); 
	XModelTreeListenerSWTSync syncListener = new XModelTreeListenerSWTSync(listener);	
	private TreeDragDropProvider dndProvider = new TreeDragDropProvider(); 
	private ControlDragDrop dnd = new ControlDragDrop();
	private XModelObjectSelectionProvider selectionProvider = new XModelObjectSelectionProvider();
	private NavigatorStatusLineProvider statusLineProvider = new NavigatorStatusLineProvider();
	
	public void init(IPageSite pageSite) {
		super.init(pageSite);
		getSite().getWorkbenchWindow().getSelectionService().addPostSelectionListener(getSelectionListener());
	}

	public void dispose() {
		super.dispose();
		model.removeModelTreeListener(syncListener);
		if (syncListener!=null) syncListener.dispose();
		syncListener = null;
		listener = null;
		if (content!=null) content.dispose();
		content = null;
		if (selectionProvider!=null) selectionProvider.dispose();
		selectionProvider = null;
		if(getSite() == null) return;
		try {
			getSite().getWorkbenchWindow().getSelectionService().removePostSelectionListener(getSelectionListener());
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}
	
	public void setCache(XModelObjectCache cache) {
		this.cache = cache;
		model = cache.getObject().getModel();		
		content.setRoot(cache);
	}
	
	public void createControl(Composite parent) {
		super.createControl(parent);
		selectionProvider.addHost("tree", getTreeViewer(), true);
		getSite().setSelectionProvider(selectionProvider);
		getTreeViewer().setContentProvider(content);
		getTreeViewer().setInput(cache);
		getTreeViewer().setLabelProvider(new NavigatorLabelProvider());
		menu.setViewer(getTreeViewer());
		if(!content.isProjectEnabled) {
			getTreeViewer().getTree().addMouseListener(menu);
			getTreeViewer().getTree().addKeyListener(menu);
		}
		listener.setViewer(getTreeViewer());
		model.addModelTreeListener(syncListener);
		dnd.setProvider(dndProvider);
		dndProvider.setTree(getTreeViewer().getTree());
		if(!content.isProjectEnabled) {
			dnd.enable();
		} else {
			dnd.enableDrag();
		}	
		initListeners(getTreeViewer());
		getTreeViewer().setExpandedState(cache.getObject(), true);
	}
	
	public void addFilter(XFilteredTreeConstraint filter) {
		content.addFilter(filter);
	}
	
	protected void initListeners(TreeViewer viewer) {
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				handleSelectionChanged(event);
			}
		});
	}
	
	protected void handleSelectionChanged(SelectionChangedEvent event) {
		IStructuredSelection sel = (IStructuredSelection) event.getSelection();
		updateStatusLine(sel);
	}

	protected void updateStatusLine(IStructuredSelection selection) {
		String msg = statusLineProvider.getStatusLineMessage(selection);
		getSite().getActionBars().getStatusLineManager().setMessage(msg);
	}
	
	public TreeViewerMenuInvoker getOutlineMenuInvoker() {
		return menu;
	}
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#getTreeViewer()
	 */
	public TreeViewer getTreeViewer() {
		// TODO Auto-generated method stub
		return super.getTreeViewer();
	}
//ds	Opportunity of disabling dnd in outline is added.
	public void disableDnd() {
		dnd.setProvider(null);
	}
//endds	
	
	private ISelectionListener fSelectionListener = null;

	private ISelectionListener getSelectionListener() {
		if (fSelectionListener == null) {
			fSelectionListener = new PostSelectionServiceListener();
		}
		return fSelectionListener;
	}

	private class PostSelectionServiceListener implements ISelectionListener {
		public void selectionChanged(IWorkbenchPart part, ISelection selection) {
			if (getControl() != null && !getControl().isDisposed() && !getControl().isFocusControl() && !selectionProvider.isFiringSelection()) {
				if(part instanceof ViewPart) {
					//WebProjectsNavigator
					return;
				}
				ISelection validContentSelection = mapSelection(getTreeViewer(), selection);
				getTreeViewer().refresh(true);
				boolean isLinked = true; ///getConfiguration().isLinkedWithEditor(getTreeViewer());
				if (isLinked) {
					getTreeViewer().setSelection(validContentSelection, true);
				}
			}
		}
	}
	
	private ISelection mapSelection(TreeViewer viewer, ISelection selection) {
		return selection;
	}
	
}
