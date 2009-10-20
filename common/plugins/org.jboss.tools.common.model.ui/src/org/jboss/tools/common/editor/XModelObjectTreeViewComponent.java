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

import java.util.*;

import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.action.ModelContributionManager;
import org.jboss.tools.common.model.ui.dnd.ControlDragDrop;
import org.jboss.tools.common.model.ui.navigator.*;
import org.jboss.tools.common.model.ui.outline.*;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.*;

public class XModelObjectTreeViewComponent {
	XModel model;
	XModelObjectCache cache;
	XModelObjectContentProvider content = new XModelObjectContentProvider();
	ILabelProvider label;
	TreeViewerMenuInvoker menu = new OutlineMenuInvoker();
	TreeViewerModelListenerImpl listener; 
	XModelTreeListenerSWTASync syncListener;	
	private TreeDragDropProvider dndProvider = new TreeDragDropProvider(); 
	private ControlDragDrop dnd = new ControlDragDrop();
	TreeViewer treeViewer = null;

	public XModelObjectTreeViewComponent() {
		this(new TreeViewerModelListenerImpl());
	}

	public void setMenuInvoker(TreeViewerMenuInvoker menu) {
//		this.menu = menu;
	}

	protected XModelObjectTreeViewComponent(TreeViewerModelListenerImpl listener) {
		this.listener = listener;
		syncListener = new XModelTreeListenerSWTASync(listener);
	}
	
	public void setCache(XModelObjectCache cache) {
		this.cache = cache;
		setModel(cache.getObject().getModel());
		content.setRoot(cache);
	}	
	
	public void setModel(XModel model) {
		if(this.model == model) return;
		if(this.model == null && treeViewer != null && treeViewer.getTree() != null && !treeViewer.getTree().isDisposed()) model.addModelTreeListener(syncListener);
		if(model == null) this.model.removeModelTreeListener(syncListener);
		this.model = model;
		if(model != null) model.addModelTreeListener(syncListener);
	}
	
	public void setModelObject(XModelObject object) {
		if(object == null && cache == null) return;
		if(object != null && cache != null && cache.getObject() == object) return;
		cache = (object == null) ? null : new XModelObjectCache(object);
		setModel((object == null) ? null : object.getModel());
		content.setRoot(cache);
		if(treeViewer != null) {
			treeViewer.setInput(cache);
		} else {
			///selectionProvider.setSelection(new StructuredSelection(object));
		}
	}
	
	public Control createControl(Composite parent, int style) {
		treeViewer = new TreeViewer(parent, style);
		treeViewer.setAutoExpandLevel(2);
		treeViewer.setLabelProvider(label = LabelDecoratorImpl.decorateLabelProvider(new NavigatorLabelProvider()));
		treeViewer.setContentProvider(content);
		treeViewer.setInput(cache == null ? null : cache);

		initContextMenu();
		menu.setViewer(treeViewer);
		treeViewer.getTree().addMouseListener(menu);

		listener.setViewer(treeViewer);
		dnd.setProvider(dndProvider);
		dndProvider.setTree(treeViewer.getTree());
		dnd.enable();
		if(cache != null) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					if(treeViewer != null) {
						ISelection s = treeViewer.getSelection();
						if(s != null && !s.isEmpty()) return;
						treeViewer.setSelection(new StructuredSelection(cache.getObject()));
					}
				}
			});
		} 
		selectionProvider.setTreeViewer(getViewer());
		return treeViewer.getTree();
	}

	protected void initContextMenu() {
		final ModelContributionManager menuMgr = new ModelContributionManager(null) {
			public XActionList getActionList(XModelObject o) {
				if(o.getFileType() != XModelObject.FILE) return super.getActionList(o);
				String ent = o.getModelEntity().getName() + "_EditorActionList"; //$NON-NLS-1$
				XModelEntity entity = o.getModel().getMetaData().getEntity(ent); 
				return (entity != null) ? entity.getActionList() : super.getActionList(o);
			}
		};
		menu.setStandardInvoker(menuMgr);
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				fillContextMenu(manager);
			}
		});
		final TreeViewer treeViewer = getViewer();
		Menu menu = menuMgr.createContextMenu(treeViewer.getTree());
		treeViewer.getTree().setMenu(menu);	
//		Display.getDefault().asyncExec(new Runnable() {
//			public void run() {
//				ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow()
//				.getActivePage().getActiveEditor().getSite()
//				.registerContextMenu(menuMgr, treeViewer);
//			}
//		});

	}
	protected void fillContextMenu(IMenuManager menu) {
		IStructuredSelection selection =
			(IStructuredSelection) getViewer().getSelection();
		if(menu instanceof ModelContributionManager) {
			((ModelContributionManager)menu).setSelection(selection);
		}
		menu.update(true);
	}
	
	public Control createControl(Composite parent) {
		return createControl(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
	}
	
	public void dispose() {
		treeViewer = null;
		disconnect();
		model = null;
		if (content != null) {
			content.dispose();
			content = null;
		}
		if (syncListener != null) {
			syncListener.dispose();
			syncListener = null;
		}
		if (listener!=null) listener.disopse();
		listener = null;
		if (selectionProvider!=null) selectionProvider.dispose();
		selectionProvider = null;
//		menu = null;
		if(label != null) {
			label.dispose();
			label = null;
		}
	}
	
//	private void connect() {
//		if(model != null) model.addModelTreeListener(syncListener);
//	}
	
	private void disconnect() {
		if(model != null) model.removeModelTreeListener(syncListener);
	}
	
	public void addFilter(XFilteredTreeConstraint filter) {
		content.addFilter(filter);
	}
	
	public TreeViewer getViewer() {
		return treeViewer;
	}

	public ISelectionProvider getSelectionProvider() {
		return selectionProvider;
	}
	
	SelectionProviderImpl selectionProvider = new SelectionProviderImpl(); 
	
	class SelectionProviderImpl implements ISelectionProvider {
		ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>();
		ISelection selection = null;  
		TreeViewer tv;
		
		public void setTreeViewer(TreeViewer tv) {
			this.tv = tv;
			ISelectionChangedListener[] cl = listeners.toArray(new ISelectionChangedListener[0]);
			for (int i = 0; i < cl.length; i++) tv.addSelectionChangedListener(cl[i]);
			if(selection != null) tv.setSelection(selection); 
		}		

		public void addSelectionChangedListener(ISelectionChangedListener listener) {
			listeners.add(listener); 
			if(tv != null) tv.addSelectionChangedListener(listener);
		}

		public ISelection getSelection() {
			return (tv == null) ? selection : tv.getSelection();
		}

		public void removeSelectionChangedListener(ISelectionChangedListener listener) {
			listeners.remove(listener); 
			if(tv != null) tv.removeSelectionChangedListener(listener);
		}

		public void setSelection(ISelection selection) {
			if(isEqualSelection(selection)) return;
			this.selection = selection; 
			if(tv != null) {
				tv.setSelection(selection);
			} else {
				for (int i = 0; i < listeners.size(); i++) {
					ISelectionChangedListener l = (ISelectionChangedListener)listeners.get(i);
					l.selectionChanged(new SelectionChangedEvent(this, selection));
				}
			}			
		}
		
		private boolean isEqualSelection(ISelection selection) {
			if(!(this.selection instanceof StructuredSelection) && 
					!(selection instanceof StructuredSelection)) return true;
			if((this.selection instanceof StructuredSelection) ||
					(selection instanceof StructuredSelection)) return false;
			return ((StructuredSelection)this.selection).getFirstElement() == ((StructuredSelection)selection).getFirstElement();
		}
		
		public void fireSelectionChanged() {
			if(listeners.isEmpty()) return;
			ISelection selection = getSelection();
///			if(formPlace != null) formPlace.setSelectedObject(getModelObject(selection));
			if(selection == null) return; 
			SelectionChangedEvent newEvent = new SelectionChangedEvent(this, selection);
			Iterator iterator = listeners.iterator();
			while (iterator.hasNext())
				((ISelectionChangedListener)iterator.next()).selectionChanged(newEvent);
		}
		
		public XModelObject getModelObject(ISelection selection) {
			if(selection == null || selection.isEmpty()) return null;
			IStructuredSelection s = (IStructuredSelection)selection;
			Object o = s.getFirstElement();
			return (o instanceof XModelObject) ? (XModelObject)o : null;
		}
		
		public void dispose() {
			if (listeners!=null) listeners.clear();
			listeners = null;
			tv = null;
		}
	}
	
}
