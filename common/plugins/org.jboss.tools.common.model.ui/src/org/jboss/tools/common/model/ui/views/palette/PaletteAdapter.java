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
package org.jboss.tools.common.model.ui.views.palette;

import java.io.File;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.ActionDeclinedException;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.IconUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.DnDUtil;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.jboss.tools.common.model.ui.views.palette.model.*;
import org.jboss.tools.common.model.ui.util.StringUtilities;

public class PaletteAdapter implements IPaletteAdapter {
	private static final int TEXT_MARGIN = 4;
	private IPalettePageAdapter viewPart = null;
	private PaletteModel model = null; 
	private ScrolledComposite pane = null;
	private PaletteModelListener modelListener = null;
	private PaletteDescriptionManager descriptionManager = null;
	private PaletteResizeManager resizeManager = null;
	private PaletteDropTargetManager dropManager = null;
	private QualifiedName persistentTabQualifiedName = new QualifiedName("", "Palette_tab"); //$NON-NLS-1$ //$NON-NLS-2$
	private String selectedTab = null;
	private boolean fWindowsFlag;
	
	public void setPaletteViewPart(IPalettePageAdapter viewPart) {
		this.viewPart = viewPart;
	}

	public void initActionBars() {
		IActionBars bars = viewPart.getActionBars();
		if(bars != null) {
			IMenuManager menuManager = bars.getMenuManager();

			bars.getToolBarManager().add(new PaletteEditAction());
			bars.getToolBarManager().add(new ShowHideTabsAction());
			if (selectedTab != null) {
				ActionContributionItem item = (ActionContributionItem)menuManager.find(selectedTab); 
				if (item != null) {
					item.getAction().run();
				} 
			} else {
				IContributionItem[] actions = menuManager.getItems();
				if (actions.length > 0 && (actions[0] instanceof ActionContributionItem)) {
					((ActionContributionItem)actions[0]).getAction().run();
				}
			}
		}
	}

	public Control createControl(Composite composite) {
		String osName = System.getProperty("os.name"); //$NON-NLS-1$
		fWindowsFlag = osName != null && osName.toUpperCase().indexOf("WINDOWS") != -1; //$NON-NLS-1$

		try {
			selectedTab = ModelUIPlugin.getWorkspace().getRoot().getPersistentProperty(persistentTabQualifiedName);
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}

		pane = new ScrolledComposite(composite, SWT.V_SCROLL);
		pane.setExpandHorizontal(true);
		pane.setExpandVertical(true);

		descriptionManager = new PaletteDescriptionManager();
		resizeManager = new PaletteResizeManager(); 
		dropManager = new PaletteDropTargetManager(); 
		
		model = PaletteModel.getInstance();
		createPaletteTabs();

//		initActionBars();

		modelListener = new PaletteModelListener();
		model.addModelTreeListener(modelListener);

		return pane;
	}
	
	private void createPaletteTabs() {
		IActionBars bars = viewPart.getActionBars();
		IMenuManager menuManager = bars.getMenuManager();
		IPaletteNode root = model.getRoot();
		IPaletteNode[] nodes = root.getChildren();

		if (nodes != null) {
			for (int i = 0; i < nodes.length; i++) {
				createPaletteTab(nodes[i], menuManager);
			}
		}
	}
	
	private void createPaletteTab(IPaletteNode node, IMenuManager manager) {
		String title = node.getTitle();
		Composite tab = new Composite(pane, SWT.NONE);
		tab.setData(node);

		RowLayout layout = new RowLayout(SWT.HORIZONTAL);
		layout.pack = true;
		layout.justify = false;
		layout.marginLeft = 0;
		layout.marginTop = 0;
		layout.marginRight = 0;
		layout.marginBottom = 0;
		layout.spacing = 0;
		tab.setLayout(layout);

		IPaletteNode[] elems = node.getChildren();
		if (elems != null) {
			for (int i = 0; i < elems.length; i++) {
				createPaletteElement(tab, elems[i]);
			}
		}
		manager.add(new PaletteTabAction(tab, title));
	}
	
	private void createPaletteElement(Composite tab, IPaletteNode node) {
		ToolBar tlb = new ToolBar(tab, SWT.HORIZONTAL | SWT.FLAT);

		ToolItem item = new ToolItem(tlb, SWT.PUSH);
		item.setText(StringUtilities.dottedString(node.getTitle(), IconUtil.PALETTE_IMAGE_WIDTH - TEXT_MARGIN, tlb));
		Image image = node.getImage();
		if (image != null) {
			image.setBackground(tlb.getBackground());
			image.setBackground(new org.eclipse.swt.graphics.Color(tlb.getDisplay(), 255, 0, 0));
			item.setImage(image);
			if (fWindowsFlag)
				item.setDisabledImage(image);
		}
		item.setData(node);
		item.addSelectionListener(new PaletteSelectionListener(node));
	}
	
	public void setEnabled(boolean enabled) {
		if (!fWindowsFlag) return;

		Control[] tabs = pane.getChildren();
		for (int i = 0; i < tabs.length; i++) {
			if (tabs[i] instanceof Composite) {
				Control[] elems = ((Composite)tabs[i]).getChildren();
				for (int j = 0; j < elems.length; j++) {
					if (elems[j] instanceof ToolBar) {
						ToolBar tlb = (ToolBar)elems[j]; 
						for (int k = 0, c = tlb.getItemCount(); k < c; k++) {
							tlb.getItem(k).setEnabled(enabled);
						}
					}
				}
			}
		}
	}

	private void reload(XModelObject xtab) {
		model.reload();

		IActionBars bars = viewPart.getActionBars();
		IMenuManager menuManager = bars.getMenuManager();
		
		String oldTitle = null;
		Control oldTab = pane.getContent();
		Control[] oldTabs = pane.getChildren();
		IContributionItem[] oldActions = menuManager.getItems();

		createPaletteTabs();

		for (int i = 0; i < oldActions.length; i++) {
			if (oldTitle == null && oldTab != null && oldActions[i] instanceof ActionContributionItem) {
				IAction action = ((ActionContributionItem)oldActions[i]).getAction();
				if ((action instanceof PaletteTabAction) && ((PaletteTabAction)action).tab == oldTab) {
					oldTitle = action.getText(); 
				}
			}
			menuManager.remove(oldActions[i]);
		}

		IAction newAction = null;
		IContributionItem[] newActions = menuManager.getItems();
		
		if (xtab != null) {
			for (int i = 0; i < newActions.length; i++) {
				if (newActions[i] instanceof ActionContributionItem) {
					IAction action = ((ActionContributionItem)newActions[i]).getAction();
					if ((action instanceof PaletteTabAction) && ((IPaletteNode)((Composite)((PaletteTabAction)action).tab).getData()).getXModelObject() == xtab) {
						newAction = action; 
						break;
					}
				}
			}
		}
		
		if (newAction == null && oldTitle != null) {
			for (int i = 0; i < newActions.length; i++) {
				if (newActions[i] instanceof ActionContributionItem) {
					IAction action = ((ActionContributionItem)newActions[i]).getAction();
					if (oldTitle.equalsIgnoreCase(action.getText())) {
						newAction = action; 
						break;
					}
				}
			}
		}
		if (newAction == null) {
			if (newActions.length > 0 && (newActions[0] instanceof ActionContributionItem)) {
				newAction = ((ActionContributionItem)newActions[0]).getAction();
			}
		}
		if (newAction != null) {
			newAction.run();
		}

		for (int i = 0; i < oldTabs.length; i++) {
			oldTabs[i].dispose();
		}
		
		setEnabled(viewPart.isEnabled());
	}

	public void dispose() {
		dropManager.dispose();
		descriptionManager.dispose();
		model.removeModelTreeListener(modelListener);
		viewPart.getActionBars().getToolBarManager().removeAll();
		try {
			if (selectedTab != null) { 
				ModelUIPlugin.getWorkspace().getRoot().setPersistentProperty(persistentTabQualifiedName, selectedTab);
			}
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

	public void setPaletteContents(PaletteContents contents) {
	}

	private class PaletteTabAction extends Action {
		private Composite tab;

		public PaletteTabAction(Composite tab, String text) {
			super(text);
			this.tab = tab;
			setId(text);
		}

		public Composite getTab() {
			return tab;
		}

		public void run() {
			if (pane.getContent() != null) {
				dropManager.dispose();
				resizeManager.dispose();
				descriptionManager.setEnabled(false);
			}
			viewPart.setContentDescription(getText());
			pane.setContent(tab);
			descriptionManager.install(tab);
			resizeManager.install(tab);
			dropManager.install(tab);
			selectedTab = getId();
		}
	}
	
	private class PaletteResizeManager extends ControlAdapter {
		Composite tab = null;
		
		public void install(Composite tab) {
			dispose();
			this.tab = tab;
			resize();
			tab.addControlListener(this);
		}

		public void dispose() {
			if (tab != null) {
				if (!tab.isDisposed()) {
					tab.removeControlListener(this);
				}
				tab = null;
			}
		}

		public void controlResized(ControlEvent event) {
			resize();
		}
		
		private void resize() {
			int width = pane.getClientArea().width;
			if (width > 0) {
				int height = tab.computeSize(width, SWT.DEFAULT).y;
				pane.setMinHeight(height);
			}
		}
	}
	
	private class PaletteSelectionListener extends SelectionAdapter {
		IPaletteNode node;
		
		public PaletteSelectionListener(IPaletteNode node) {
			super();
			this.node = node;
		}

		public void widgetSelected(SelectionEvent event) {
			viewPart.insertIntoEditor(((PaletteElement)node).getXModelObject());
		}
	}

	private class PaletteDropTargetManager extends DropTargetAdapter {
		private DropTarget target = null;

	
		public void install(Composite tab) {
			dispose();
			Transfer[] types = new Transfer[]{
					ModelTransfer.getInstance(),
					FileTransfer.getInstance()
			};
			target = new DropTarget(tab, DND.DROP_MOVE | DND.DROP_COPY);
			target.setTransfer(types);
			target.addDropListener(this);		
		}

		public void dispose() {
			if (target != null) {
				if(!target.isDisposed()) {
					target.removeDropListener(this);
					target.dispose();
				} 		
				target = null;
			}
		}

		public void dragOver(DropTargetEvent event) {
			XModelObject o = null;
			if(FileTransfer.getInstance().isSupportedType(event.currentDataType)) {
				File f = getFile(event);
				if(f != null) {
					IFile ef = EclipseResourceUtil.getFile(f.getAbsolutePath());
					o = (ef == null) ? null : EclipseResourceUtil.getObjectByResource(ef);
				}
			} else if (event.currentDataType.type != ModelTransfer.MODEL_ID) {
				o = null;
			} else {
				o = getModelObject(event.x, event.y);
			}
			boolean enabled = o != null && DnDUtil.isPasteEnabled(o); 
			event.detail = enabled ? DND.DROP_COPY : DND.DROP_NONE;
		}

		public void drop(DropTargetEvent event) {
			XModelObject o = null;
			if(FileTransfer.getInstance().isSupportedType(event.currentDataType)) {
				File f = getFile(event);
				if(f != null) {
					IFile ef = EclipseResourceUtil.getFile(f.getAbsolutePath());
					o = (ef == null) ? null : EclipseResourceUtil.getObjectByResource(ef);
				}
			} else if(event.currentDataType.type != ModelTransfer.MODEL_ID) {
				o = null;
			} else {
				o = getModelObject(event.x, event.y);
			}
			if(o != null) {
				try {
					DnDUtil.paste(o, new Properties());
					model.getXModel().saveOptions();
				} catch (ActionDeclinedException ade) {
					//ignore - this exception is thrown to inform that user 
					//selected cancel option in dialog. 
				} catch (XModelException e) {
					message(e);
				}
			} else {
				event.detail = DND.DROP_NONE;
			}
		}
		
		private XModelObject getModelObject(int x, int y) {
			IPaletteNode node = null;
			Composite tab = (Composite)target.getControl();
			Control[] controls = tab.getChildren();
			for (int i = 0; i < controls.length; i++) {
				if (controls[i] instanceof ToolBar) {
					Rectangle area = controls[i].getBounds();
					if (area.contains(x, y)) {
						ToolItem item = ((ToolBar)controls[i]).getItem(new Point(x - area.x, y - area.y));
						if (item != null) {
							node = (IPaletteNode)item.getData(); 
						}
						break;
					}
				}
			}
			if (node == null) {
				node = (IPaletteNode)tab.getData();
			}
			return node.getXModelObject();
		}

		private void message(Exception e) {
			ModelUIPlugin.getPluginLog().logError( e);
		}

	
		private File getFile(DropTargetEvent event) {
			String[] s = (String[])event.data;
			return(s == null || s.length == 0) ? null : new File(s[0]);
		}
	}	
	
	private class PaletteModelListener implements XModelTreeListener {
		private boolean isTransaction = false;
		private boolean isDirty = false;
		private XModelObject lastAddedTab = null;
	
		public void nodeChanged(XModelTreeEvent event) {
			run(event);
		}

		public void structureChanged(XModelTreeEvent event) {
			run(event);
		}

		private void run(XModelTreeEvent event) {
			if("transaction_begin".equals(event.getInfo())) { //$NON-NLS-1$
				isTransaction = true;
				return;
			}
			if("transaction_end".equals(event.getInfo())) { //$NON-NLS-1$
				isTransaction = false;
				if(isDirty) {
					isDirty = false;
					reload(lastAddedTab);
					lastAddedTab = null; 
				}
				return;
			}
			XModel xmodel = model.getXModel();
			XModelObject exo = event.getModelObject();
			boolean q = event.kind() == XModelTreeEvent.STRUCTURE_CHANGED && xmodel.getRoot().getPath().equals(exo.getPath());
			XModelObject xroot = xmodel.getRoot("Palette"); //$NON-NLS-1$
			if (xroot == null || isExist(xroot, event.getModelObject()) || q) {
				if (event.kind() == XModelTreeEvent.CHILD_ADDED && xroot != null && xroot.getPath().equals(exo.getPath())) {
					Object info = event.getInfo();
					if (info instanceof XModelObject) {
						lastAddedTab = (XModelObject)info; 
					}
				}
				if(isTransaction) {
					isDirty = true;
				} else {
					reload(lastAddedTab);
					lastAddedTab = null; 
				}
			}
		}

		private boolean isExist(XModelObject root, XModelObject modelObject) {
			if (root == null || modelObject==null) {
				return false;
			}
			if (modelObject.getPath() == null || root.getPath() == null) {
				return false;
			}
			return (modelObject.getPath() + "/").startsWith(root.getPath() + "/"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
	
	private class PaletteEditAction extends Action {
		public PaletteEditAction() {
			super("Palette Editor", model.createImageDescriptor("palette_editor.gif")); //$NON-NLS-2$
			setToolTipText("Palette Editor");
		}
		
		public void run() {
			model.openEditor(pane.getShell());			
		}
	}
	
	private class ShowHideTabsAction extends Action {
		public ShowHideTabsAction() {
			super("Show/Hide tabs", model.createImageDescriptor("show-hide.gif")); //$NON-NLS-2$
			setToolTipText("Show/Hide");
		}
		public void run() {
			model.runShowHideDialog();			
		}
	}
}
