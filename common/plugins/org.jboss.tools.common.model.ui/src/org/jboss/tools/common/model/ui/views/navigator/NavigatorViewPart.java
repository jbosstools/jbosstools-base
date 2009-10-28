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

import java.util.Properties;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.commands.ActionHandler;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.ISaveablePart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.BuildAction;
import org.eclipse.ui.actions.CloseResourceAction;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.ISetSelectionTarget;
import org.eclipse.ui.part.IShowInTarget;
import org.eclipse.ui.part.ShowInContext;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.framelist.FrameList;
import org.eclipse.ui.views.navigator.IResourceNavigator;
import org.eclipse.ui.views.navigator.ResourceComparator;
import org.eclipse.ui.views.navigator.ResourceNavigatorActionGroup;
import org.eclipse.ui.views.navigator.ResourcePatternFilter;
import org.eclipse.ui.views.navigator.ResourceSorter;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XJob;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XJob.XRunnable;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.impl.trees.FileSystemsTree;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.action.ModelContributionManager;
import org.jboss.tools.common.model.ui.dnd.ControlDragDrop;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.navigator.NavigatorLabelProvider;
import org.jboss.tools.common.model.ui.navigator.NavigatorStatusLineProvider;
import org.jboss.tools.common.model.ui.navigator.TreeViewerDragDropProvider;
import org.jboss.tools.common.model.ui.navigator.TreeViewerMenuInvoker;
import org.jboss.tools.common.model.ui.navigator.TreeViewerModelListenerImpl;
import org.jboss.tools.common.model.ui.select.XModelObjectSelectionProvider;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class NavigatorViewPart extends ViewPart implements ISaveablePart, ISetSelectionTarget, IResourceNavigator , IShowInTarget //, ISelectionProvider 
{
	public static final String VIEW_ID = "org.jboss.tools.common.model.ui.navigator.NavigatorViewPart"; //$NON-NLS-1$
	private TreeViewer viewer;
	private XModelObjectSelectionProvider selectionProvider = new XModelObjectSelectionProvider();
	private TreeViewerDragDropProvider dndProvider = new TreeViewerDragDropProvider(); 
	private ControlDragDrop dnd = new ControlDragDrop();
	private CloseResourceAction closeProjectAction;
	private BuildAction buildProjectAction;
	private BuildAction rebuildProjectAction;
	protected NavigatorContentProvider contentProvider;
	private IMemento memento = null;
	private NavigatorStatusLineProvider statusLineProvider = new NavigatorStatusLineProvider();
	private ActivationListener fActivationListener= new ActivationListener();
	
	boolean useModelMenu = true;
	
	public NavigatorViewPart() {
		selectionProvider.addSelectionChangedListener(new SL());
	}
	
	public void setFocus() {
		getViewer().getTree().setFocus();
	}

	public TreeViewer getViewer() {
		return viewer;
	}
	
	public void createPartControl(Composite parent)	{
		createViewer(parent);
		viewer.setInput(getInitialInput());
		selectionProvider.addHost("viewer", viewer, true); //$NON-NLS-1$
		getSite().setSelectionProvider(selectionProvider);
		if (memento != null) {
			restoreLinkingEnabled();
		}
		initActionBars();
		dndProvider.setViewer(viewer);
		dnd.setProvider(dndProvider);
		dnd.enable();
		initContextMenu();

		activateF3();

		getSite().getPage().addPartListener(partListener);

		if (memento != null)
			restoreState(memento);
		memento = null;
	}
	
	private void activateF3() {
		IHandlerService h = (IHandlerService)getViewSite().getActionBars().getServiceLocator().getService(IHandlerService.class);
		IAction action = new Action(){
			public boolean isEnabled() {
				return true;
			}
			public void run() {
				ISelection s = viewer.getSelection();
				if(s.isEmpty() || !(s instanceof StructuredSelection)) return;
				StructuredSelection ss = (StructuredSelection)s;
				Object o = ss.getFirstElement();
				if(!(o instanceof XModelObject)) return;
				XModelObject object = (XModelObject)o;
				if(XActionInvoker.getAction("Open", object) == null) return; //$NON-NLS-1$
				XActionInvoker.invoke("Open", object, null);					 //$NON-NLS-1$
			}
		};
		h.activateHandler("org.eclipse.jdt.ui.edit.text.java.open.editor", new ActionHandler(action)); //$NON-NLS-1$
	}
	
	private IAction createActionInstance(String name) {
		return (IAction)ModelFeatureFactory.getInstance().createFeatureInstance(name);
	}
	
	protected String[] getActionClasses() {
		String[] actions = new String[]{
		};
		return actions;
	}

	protected void initActionBars() {
		IActionBars bars = getViewSite().getActionBars();
        String[] actions = getActionClasses();
        for (int i = 0; i < actions.length; i++) {
			IAction action = createActionInstance(actions[i]);
        	if(action != null) bars.getToolBarManager().add(action);
        }
		closeProjectAction = new CloseResourceAction(getViewer().getTree().getShell());
		getSite().getSelectionProvider().addSelectionChangedListener(closeProjectAction);
		getViewSite().getActionBars().setGlobalActionHandler(
				IDEActionFactory.CLOSE_PROJECT.getId(),
			closeProjectAction
		);
		buildProjectAction = new BuildAction(getViewer().getTree().getShell(), IncrementalProjectBuilder.INCREMENTAL_BUILD);
		getSite().getSelectionProvider().addSelectionChangedListener(buildProjectAction);
		getViewSite().getActionBars().setGlobalActionHandler(
				IDEActionFactory.BUILD_PROJECT.getId(),
			buildProjectAction
		);
		rebuildProjectAction = new BuildAction(getViewer().getTree().getShell(), IncrementalProjectBuilder.FULL_BUILD);
		getSite().getSelectionProvider().addSelectionChangedListener(rebuildProjectAction);
		getViewSite().getActionBars().setGlobalActionHandler(
				IDEActionFactory.BUILD_PROJECT.getId(),
			rebuildProjectAction
		);
		makeActions();
		getActionGroup().fillActionBars(getViewSite().getActionBars());
		updateActionBars((IStructuredSelection) viewer.getSelection());
	}
	
	public void dispose() {
		if (getSite()!=null&&getSite().getSelectionProvider()!=null) {
			getSite().getSelectionProvider().removeSelectionChangedListener(closeProjectAction);	
			getSite().getSelectionProvider().removeSelectionChangedListener(buildProjectAction);	
			getSite().getSelectionProvider().removeSelectionChangedListener(rebuildProjectAction);
		}
		if(getSite()!=null) {
		    getSite().getWorkbenchWindow().getShell().removeShellListener(fActivationListener);
		}
		super.dispose();
	}

	protected void createViewer(Composite parent) {
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.setUseHashlookup(true);
		initContentProvider(viewer);
		initLabelProvider(viewer);
//		initFilters(viewer);
		initListeners(viewer);
	}
	
	protected void initLabelProvider(TreeViewer treeViewer)	{
		treeViewer.setLabelProvider(new NavigatorLabelProvider());		
	}
	
	protected void initContentProvider(TreeViewer treeViewer) {
		NavigatorContentProvider c = new NavigatorContentProvider();
		ML listener = new ML();
		listener.setViewer(treeViewer);
		c.setListener(listener);
		contentProvider = c;
		treeViewer.setContentProvider(contentProvider);
	}

	protected void initListeners(TreeViewer treeViewer) {
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				handleSelectionChanged(event);
			}
		});
//@S_CHECK@
  		listener = createMenuInvoker();
		listener.setViewer(viewer);
		if(useModelMenu) {
			viewer.getTree().addMouseListener(listener);
		}
	}
	
	protected TreeViewerMenuInvoker createMenuInvoker() {
		return new NavigatorMenuInvoker();
	}

	public XModelObject getSelectedModelObject() {
		if(getViewer() == null || getViewer().getTree() == null || getViewer().getTree().isDisposed()) return null;
		TreeItem[] ti = getViewer().getTree().getSelection();
		if(ti == null || ti.length == 0) return null;
		return getModelObjectForWidget(ti[0]);
	}
	
	public XModelObject getModelObjectForWidget(Widget widget) {
		if(!(widget instanceof TreeItem)) return null;
		TreeItem item = (TreeItem)widget;
		Object itemData = item.getData();
		return (!(itemData instanceof XModelObject)) ? null : (XModelObject)itemData;
	}
	
	protected IAdaptable getInitialInput() {
		IAdaptable input = getSite().getPage().getInput();
		if (input != null) {
			IResource resource = null;
			if (input instanceof IResource) {
				resource = (IResource) input;
			} else {
				resource = (IResource) input.getAdapter(IResource.class);
			}
			if (resource != null) {
				switch (resource.getType()) {
					case IResource.FILE :
						return resource.getParent();
					case IResource.FOLDER :
					case IResource.PROJECT :
					case IResource.ROOT :
						return (IContainer) resource;
					default :
						// Unknown resource type.  Fall through.
						break;
				}
			}
		}
		return ModelUIPlugin.getWorkspace().getRoot();
	}
	
	public Object getAdapter(Class adapter)
	{
		//ModelUIPlugin.log("getAdapter(" + adapter.getName() + ")");
		return super.getAdapter(adapter);
	}

	public void doSave(IProgressMonitor monitor) {
		XModelObject o = getSelectedModelObject();
		if(o == null) return;
		while(o != null && o.getFileType() == XFileObject.NONE) o = o.getParent();
		if(o == null || o.getFileType() != XFileObject.FILE) return;
		XActionInvoker.invoke("SaveActions.Save", o, null);		 //$NON-NLS-1$
		firePropertyChange(IEditorPart.PROP_DIRTY);
	}

	public void doSaveAs() {}

	public boolean isDirty() {
		XModelObject o = getSelectedModelObject();
		if(o == null) return false;
		while(o != null && o.getFileType() == XFileObject.NONE) o = o.getParent();
		if(o == null || o.getFileType() != XFileObject.FILE) return false;
		return o.isModified();
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public boolean isSaveOnCloseNeeded() {
		return false;
	}
	
	class SL implements ISelectionChangedListener {
		public void selectionChanged(SelectionChangedEvent event) {
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}
	
	class ML extends TreeViewerModelListenerImpl {

		public void nodeChanged(XModelTreeEvent event) {
			super.nodeChanged(event);
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}

	public void selectReveal(ISelection selection)
	{
		if (selection instanceof IStructuredSelection)
		{
			Object object = ((IStructuredSelection)selection).getFirstElement();
			if (object instanceof IProject)
			{
				XModelObject root = contentProvider.getRootByProject((IProject)object);
				if(root != null) getSite().getSelectionProvider().setSelection(
					new StructuredSelection(root)
				);
			}
		}
	}

	public void init(IViewSite site, IMemento _memento) throws PartInitException {
		init(site);
		this.memento = _memento;
		site.getWorkbenchWindow().getShell().addShellListener(fActivationListener);
	}
		
	public void saveState(IMemento _memento) {
		XModelObject o = getSelectedModelObject();
		if(o != null) _memento.putString("selection", getNodePath(o)); //$NON-NLS-1$
		saveLinkingEnabled(_memento);
	}

	public void restoreState(IMemento _memento) {
		String selection = _memento.getString("selection"); //$NON-NLS-1$
		XModelObject o = findModelObject(selection);
		if(o != null) viewer.setSelection(new StructuredSelection(o));
		restoreLinkingEnabled(); 
	}
	
	private String getNodePath(XModelObject o) {
		IProject p = (IProject)o.getModel().getProperties().get("project"); //$NON-NLS-1$
		return p.getName() + ":" + o.getPath(); //$NON-NLS-1$
	}
	
	private XModelObject findModelObject(String nodepath) {
		if(nodepath == null || nodepath.length() == 0) return null;
		int i = nodepath.indexOf(":"); //$NON-NLS-1$
		IProject p = ModelUIPlugin.getWorkspace().getRoot().getProject(nodepath.substring(0, i));
		return EclipseResourceUtil.getObjectByPath(p, nodepath.substring(i + 1));
	}

	// see ResourceNavigator	
	protected void handleSelectionChanged(SelectionChangedEvent event) {
		IStructuredSelection sel = (IStructuredSelection) event.getSelection();
		updateStatusLine(sel);
		updateActionBars(sel);
		linkToEditor(sel);
	}

	protected void updateStatusLine(IStructuredSelection selection) {
		String msg = statusLineProvider.getStatusLineMessage(selection);
		getViewSite().getActionBars().getStatusLineManager().setMessage(msg);
	}
	
	//// IResourceNavigator

	public ResourcePatternFilter getPatternFilter() {
		return new ResourcePatternFilter();
	}
	
	IWorkingSet workingSet = null;

	public IWorkingSet getWorkingSet() {
		return workingSet;
	}

	public void setWorkingSet(IWorkingSet workingSet) {
		this.workingSet = workingSet;
	}
	
	protected ResourceSorter resourceSorter = null;
	public ResourceSorter getSorter() {
		return resourceSorter;
	}

	public void setSorter(ResourceSorter sorter) {
		resourceSorter = sorter;
	}
	
	String[] filtersPreference = new String[0];

	public void setFiltersPreference(String[] patterns) {
		filtersPreference = patterns;
	}

	public FrameList getFrameList() {
		return null;
	}

	protected boolean isLinkingEnabled = false;

	public boolean isLinkingEnabled() {
		return isLinkingEnabled;
	}

	public void setLinkingEnabled(boolean enabled) {
		isLinkingEnabled = enabled;
		if (enabled) {
			IEditorPart editor = getSite().getPage().getActiveEditor();
			if (editor != null && editor != this) {
				editorActivated(editor);
			}
		}
	}
	
	protected void makeActions() {
		setActionGroup(new ModelNavigatorActionGroup(this));
	}
	
	ResourceNavigatorActionGroup actionGroup;

	protected ResourceNavigatorActionGroup getActionGroup() {
		return actionGroup;
	}

	protected void setActionGroup(ResourceNavigatorActionGroup actionGroup) {
		this.actionGroup = actionGroup;
	}

	protected void updateActionBars(IStructuredSelection selection) {
		ResourceNavigatorActionGroup group = getActionGroup();
		if (group != null) {
			group.setContext(new ActionContext(selection));
			group.updateActionBars();
		}
	}
	
	// MenuManager
	
	ModelContributionManager menuMgr;
	TreeViewerMenuInvoker listener;
	
	protected void initContextMenu() {
		menuMgr = new ModelContributionManager(getSite().getShell(), listener);
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				fillContextMenu(manager);
			}
		});
		TreeViewer treeViewer = getViewer();
		Menu menu = menuMgr.createContextMenu(treeViewer.getTree());
		treeViewer.getTree().setMenu(menu);
		getSite().registerContextMenu(menuMgr, treeViewer);
		listener.setStandardInvoker(menuMgr);

	}
	
	public void callMenu() {
		fillContextMenu(menuMgr);
		Menu menu = menuMgr.createContextMenu(viewer.getTree());
		menu.setVisible(true);
	}

	protected void fillContextMenu(IMenuManager menu) {
//		IStructuredSelection selection =
//			(IStructuredSelection) getViewer().getSelection();
//		if(menu instanceof ModelContributionManager) {
//			((ModelContributionManager)menu).setSelection(selection);
//		}
		menu.update(true);
	}
	
	// linking
	
	public TreeViewer getTreeViewer() {
		return (TreeViewer)getViewer();
	}
	
	private IPartListener partListener = new IPartListener() {
		public void partActivated(IWorkbenchPart part) {
			if (part instanceof IEditorPart && part != NavigatorViewPart.this)
				editorActivated((IEditorPart) part);
		}
		public void partBroughtToTop(IWorkbenchPart part) {
		}
		public void partClosed(IWorkbenchPart part) {
		}
		public void partDeactivated(IWorkbenchPart part) {
		}
		public void partOpened(IWorkbenchPart part) {
		}
	};
	
	private void saveLinkingEnabled(IMemento _memento) {
		_memento.putInteger("LinkModelProjectsToEditor", isLinkingEnabled ? 1 : 0); //$NON-NLS-1$
	}
	
	private void restoreLinkingEnabled() {
		Integer val = memento.getInteger("LinkModelProjectsToEditor");//IWorkbenchPreferenceConstants.LINK_NAVIGATOR_TO_EDITOR); //$NON-NLS-1$
		if (val != null) {
			isLinkingEnabled = val.intValue() != 0;
		}
	}
	
	protected void editorActivated(IEditorPart editor) {
		if (!isLinkingEnabled()) {
			return;
		}
		IEditorInput input = editor.getEditorInput();
		XModelObject o = null;
		if(input instanceof IModelObjectEditorInput) {
			o = ((IModelObjectEditorInput)input).getXModelObject();
		} else if (input instanceof IFileEditorInput) {
			IFileEditorInput fileInput = (IFileEditorInput) input;
			IFile file = fileInput.getFile();
			if(file == null) return;
			o = getObjectByResource(file);
		} 
		if(o == null) return;
		ISelection newSelection = new StructuredSelection(o);
		if (!getTreeViewer().getSelection().equals(o)) {
			getTreeViewer().setSelection(newSelection);
		}
	}
	
	/**
	 * Returns model object associated with file in tree. 
	 * Navigator may display model object that is not
	 * the model object representing the file. E.g. see
	 * bean objects in jsf 
	 * @param file
	 * @return
	 */	
	protected XModelObject getObjectByResource(IFile file) {
		XModelObject o = EclipseResourceUtil.getObjectByResource(file);
		if(o == null || contentProvider == null) return o;
		XFilteredTree t = contentProvider.getFilteredTree(o);
		if(!(t instanceof FileSystemsTree)) return o;
		FileSystemsTree fst = (FileSystemsTree)t;
		return fst.getRepresentation(o);
	}

	protected void linkToEditor(IStructuredSelection selection) {
		if (!isLinkingEnabled()) {
			return;
		}
		Object obj = selection.getFirstElement();
		if (obj instanceof IAdaptable && selection.size() == 1) {
			IFile file = (IFile) ((IAdaptable)obj).getAdapter(IFile.class);
			if(file == null && obj instanceof XModelObject) {
				XModelObject o = (XModelObject)obj;
				Properties p = new Properties();
				p.setProperty("onlySelectIfOpen", "true"); //$NON-NLS-1$ //$NON-NLS-2$
				if(XActionInvoker.getAction("Open", o) != null) { //$NON-NLS-1$
					XActionInvoker.invoke("Open", o, p); //$NON-NLS-1$
				}
				return;
			}
			IWorkbenchPage page = getSite().getPage();
			//Using internal WorkbenchPage. Must change.
			IEditorPart editor = page.findEditor(new FileEditorInput(file));
			if(editor != null) {
				page.bringToTop(editor);
				return;
			}
		}
	}
	
	boolean isHandlingActivation = false;

	class ActivationListener extends ShellAdapter {
		
		public void shellActivated(ShellEvent event) {
			if(isHandlingActivation) return;
			event.widget.getDisplay().asyncExec(new Runnable() {
				public void run() {
					XJob.addRunnable(new ActivationXRunnable());
				}
			});
		}
		
	};
	private void handleActivation() throws XModelException {
		if(isHandlingActivation) return;
		isHandlingActivation = true;
		IProject[] ps = null;
		ps = ModelUIPlugin.getWorkspace().getRoot().getProjects();
		if(ps == null) return;
		for (int i = 0; i < ps.length; i++) {
			IModelNature n = EclipseResourceUtil.getModelNature(ps[i]);
			if(n != null) n.getModel().update();
		}
		isHandlingActivation = false;
	}

	class ActivationXRunnable implements XRunnable {

		public String getId() {
			return "Activation"; //$NON-NLS-1$
		}

		public void run() {
			try {
				handleActivation();
			} catch (XModelException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
		}
		
	}
	
	public ResourceComparator getComparator() {
		return null; // TODO-3.3: does the contract allow you to not obey the get/set of comparator ?!
	}

	public void setComparator(ResourceComparator comparator) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.IShowInTarget#show(org.eclipse.ui.part.ShowInContext)
	 */
	public boolean show(ShowInContext context) {

		Object input = context.getInput();
		XModelObject o = null;
		if (input instanceof IModelObjectEditorInput) {
			o = ((IModelObjectEditorInput) input).getXModelObject();
		} else if (input instanceof IFileEditorInput) {
			IFileEditorInput fileInput = (IFileEditorInput) input;
			IFile file = fileInput.getFile();
			if (file == null)
				return false;
			o = getObjectByResource(file);
		}
		if (o == null)
			return false;
		ISelection newSelection = new StructuredSelection(o);
		if (!getTreeViewer().getSelection().equals(o)) {
			getTreeViewer().setSelection(newSelection);
		}

		return true;

	}

}
