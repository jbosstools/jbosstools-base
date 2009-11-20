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
package org.jboss.tools.common.model.ui.views.navigator;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.project.ModelNature;
import org.jboss.tools.common.model.ui.navigator.TreeViewerModelListenerImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.XModelTreeListenerSWTASync;

public class NavigatorContentProvider implements ITreeContentProvider, IResourceChangeListener {
	protected FilteredTreesCache filteredTrees = FilteredTreesCache.getInstance();
	protected TreeViewer viewer = null;
	protected TreeViewerModelListenerImpl listener;
	protected XModelTreeListenerSWTASync syncListener;
	protected Set<String> projects = new HashSet<String>();
	protected int isLoading = 0;
	public NavigatorContentProvider() {}
	
	public void dispose() {
		if (viewer != null) {
			IWorkspace workspace = getWorkspace(viewer.getInput());
			if(workspace != null) {
				workspace.removeResourceChangeListener(this);
			}
		}
		if (projects != null) {
			projects.clear();
			projects = null;
		}
		if(filteredTrees != null && syncListener != null) {
			filteredTrees.removeListener(syncListener);
		}
		syncListener = null;
	}
	
	public void setListener(TreeViewerModelListenerImpl listener) {
		this.listener = listener;
		syncListener = new XModelTreeListenerSWTASync(listener);
	}
	
	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof XModelObject) {
			XFilteredTree tree = getFilteredTree(parentElement);
			return (tree == null) ? null : getFilteredTree(parentElement).getChildren((XModelObject)parentElement);
		}			
		return null;
	}
	
	public Object getParent(Object element)	{
		if (element instanceof XModelObject) {
			XFilteredTree tree = getFilteredTree(element);
			return (tree == null) ? null : tree.getParent((XModelObject)element);
		}
		return null;
	}
	
	public boolean hasChildren(Object element) {
		if (element instanceof XModelObject) {
		    XModelObject o = (XModelObject)element;
		    if(!o.isActive()) return false;
			XFilteredTree tree = getFilteredTree(element);
			return tree != null && tree.hasChildren((XModelObject)element);
		}
		return false;
	}
	
//	============================================ IStructuredContentProvider	
	public Object[] getElements(Object inputElement) {
		isLoading++;
		try {
		this.projects.clear();
		TreeMap<String,XModelObject> result = new TreeMap<String,XModelObject>(new C());
		if (inputElement instanceof IWorkspaceRoot) {
			IWorkspaceRoot workspaceRoot = (IWorkspaceRoot)inputElement;
			IProject[] projects = workspaceRoot.getProjects();
			for (int i = 0; i < projects.length; i++) {
				XModelObject root = addProject(projects[i]);
				if (root != null) result.put(projects[i].getName(), root);
			}
		} else if (inputElement instanceof IProject) {
			IProject p = (IProject)inputElement;
			XModelObject root = addProject(p);
			if (root != null) result.put(p.getName(), root);
		}
		return result.values().toArray();
		} finally {
			isLoading--;
		}
	}
	
//	============================================ IContentProvider	
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		this.viewer = (TreeViewer)viewer;
		IWorkspace oldWorkspace = getWorkspace(oldInput);
		IWorkspace newWorkspace = getWorkspace(newInput);

		if (oldWorkspace != newWorkspace) {
			if (oldWorkspace != null) 
				oldWorkspace.removeResourceChangeListener(this);
			if (newWorkspace != null)
				newWorkspace.addResourceChangeListener(this);
		}
	}
	
	private IWorkspace getWorkspace(Object input) {
		return (input instanceof IWorkspace) ? (IWorkspace)input
				: (input instanceof IContainer) ? ((IContainer)input).getWorkspace()
				: null;
	}
	
//	============================================ IResourceChangeListener
	public void resourceChanged(final IResourceChangeEvent event) {
		if(isLoading > 0) return;
		final IResourceDelta delta = event.getDelta();
		final IResource resource = event.getResource();
		Control ctrl = viewer.getControl();
		
		if (ctrl != null && !ctrl.isDisposed()) {
			// Do a sync exec, not an async exec, since the resource delta
			// must be traversed in this method.  It is destroyed
			// when this method returns.
			ctrl.getDisplay().syncExec(
				new Runnable() {
					public void run() {
						switch (event.getType()) {
							case IResourceChangeEvent.POST_CHANGE :
								handlePostChange(delta);
								break;
							case IResourceChangeEvent.PRE_CLOSE :
								handlePreClose(resource);
								break;
							case IResourceChangeEvent.PRE_DELETE :
								handlePreDelete(resource);
								break;
						}
						check();
					}
				}
			);
		}
	}
	
	private XModelObject addProject(IProject project)	{
		if (project == null || !project.isOpen()) return null;
		IModelNature nature = getModelNature(project);
		if(nature == null) return null;
		XModelObject root = nature.getModel().getRoot();
			//Does not work now that the check above excludes closed projects. 
			if(!project.isOpen()) {
				root = root.copy(0);
			}
		XFilteredTree tree = getFilteredTree(root);
		if(tree == null) return null;
		if(listener != null) {
			listener.setViewer(viewer);
			filteredTrees.addListener(syncListener, nature.getModel());
		}
		projects.add(project.getLocation().toString());
		return tree.getRoot();
	}
	
	protected IModelNature getModelNature(IProject project) {
		return EclipseResourceUtil.getModelNature(project);
	}

	protected String getFilteredTreeName(XModel model) {
		return getFilteredTreeName();
	}
	
	protected String getFilteredTreeName() {
		return "FileSystems"; //$NON-NLS-1$
	}
	
	XFilteredTree getFilteredTree(Object object) {
		if (object instanceof XModelObject)	{
			XModel model = ((XModelObject)object).getModel();
			if(FileSystemsHelper.getFileSystems(model) == null) return null;
			String name = getFilteredTreeName(model);
			return filteredTrees.getFilteredTree(name, model);
		}				
		return null;
	}
		
	private void handlePostChange(IResourceDelta delta) {
		Control ctrl = viewer.getControl();
		if (ctrl == null || ctrl.isDisposed()) return;

		IResourceDelta[] affectedChildren = delta.getAffectedChildren(IResourceDelta.ADDED | IResourceDelta.CHANGED);
		if (affectedChildren.length > 0) {
			ArrayList<XModelObject> affected = new ArrayList<XModelObject>();
			for (int i = 0; i < affectedChildren.length; i++) {
				IProject project = (IProject)affectedChildren[i].getResource().getAdapter(IProject.class);
				if (affectedChildren[i].getKind() == IResourceDelta.ADDED ||
					(affectedChildren[i].getKind() == IResourceDelta.CHANGED && 
					 ((affectedChildren[i].getFlags() & IResourceDelta.OPEN) != 0)) || 
					 !projects.contains(project.getLocation().toString())) {
					if(!ModelNature.checkModelNature(project)) {
						continue;
					}
					XModelObject root = addProject(project);
					if (root != null) affected.add(root);
				}
			}
			
			if (!affected.isEmpty()) {
				viewer.add(viewer.getInput(), affected.toArray());
				viewer.refresh();
			}
		}
	}

	private void handlePreClose(IResource resource)	{
		handlePreDelete(resource);		
	}

	private void handlePreDelete(IResource resource) {
		Control ctrl = viewer.getControl();
		if (ctrl == null || ctrl.isDisposed()) return;

		IProject project = (IProject)resource.getAdapter(IProject.class);
		if(project == null) return;
		XModelObject root = getRootByProject(project);
		projects.remove(project.getLocation().toString());
		if (root != null) {
			root.getModel().removeModelTreeListener(syncListener);
			filteredTrees.remove(root.getModel());
			viewer.remove(root);
		} 
	}

	protected XModelObject getRootByProject(IProject project) {
		if(project == null || !project.isOpen()) return null;
		IModelNature nature = EclipseResourceUtil.getModelNature(project);
		if(nature == null) return null;
		XFilteredTree filteredTree = getFilteredTree(nature.getModel().getRoot());
		return (filteredTree != null) ? filteredTree.getRoot() : null;
	}
	
	private void check() {
		if(viewer == null) return;
		Tree swtTree = viewer.getTree();
		if(swtTree == null || swtTree.isDisposed()) return;
		TreeItem[] is = swtTree.getItems();
		if(is == null || is.length == 0) return;
		for (int i = 0; i < is.length; i++) {
			XModelObject o = (XModelObject)is[i].getData();
			if(o == null) continue;
			IProject p = EclipseResourceUtil.getProject(o);
			IModelNature nature = EclipseResourceUtil.getModelNature(p);
			if(nature == null) {
				o.getModel().removeModelTreeListener(syncListener);
				viewer.remove(o);
				if(p!=null && p.exists() && p.getLocation()!=null)
				projects.remove(p.getLocation().toString());
			} else {
				String classname = o.getModel().getMetaData().getMapping("FilteredTrees").getValue(getFilteredTreeName(o.getModel())); //$NON-NLS-1$
				XFilteredTree tree = getFilteredTree(o.getModel().getRoot());
				if(tree != null && !tree.getClass().getName().equals(classname)) {
					o.getModel().removeModelTreeListener(syncListener);
					projects.remove(p.getLocation().toString());
					viewer.remove(o);
					viewer.refresh();
				}
			}
		}
	}
	
	class C implements Comparator<Object> {
		public int compare(Object o1, Object o2) {
			return (o1 == null ? "" : o1.toString()).compareToIgnoreCase(o2 == null ? "" : o2.toString()); //$NON-NLS-1$ //$NON-NLS-2$
		}
		
	}
	
}
