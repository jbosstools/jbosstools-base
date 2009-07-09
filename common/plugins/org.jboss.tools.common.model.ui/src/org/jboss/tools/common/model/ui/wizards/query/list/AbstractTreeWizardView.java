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
package org.jboss.tools.common.model.ui.wizards.query.list;

import java.util.Properties;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public abstract class AbstractTreeWizardView extends AbstractQueryWizardView {
	protected CommandBar allBar = new CommandBar();
	protected TreeViewer treeViewer;
	protected CheckTreeProvider provider = new CheckTreeProvider();
	protected TreeItemSelectionManager treeSelectionManager;
	protected String[][] vs = new String[0][];
	CheckObject object = new CheckObject(null, new String[]{"", "yes"}); //$NON-NLS-1$ //$NON-NLS-2$
	int expandingLevel = 2;
	
	public AbstractTreeWizardView() {
		createAllBar();
	}

	public void dispose() {
		super.dispose();
		if (allBar!=null) allBar.dispose();
		allBar = null;
	}
	
	protected abstract String[] getActions();
	
	protected void createAllBar() {
		allBar.getLayout().direction = SWT.VERTICAL;
		allBar.setCommands(getActions());
		allBar.addCommandBarListener(this);
	}
	
	public void setObject(Object data) {
		super.setObject(data);
		Properties p = (Properties)data;
		vs = (String[][])p.get("data"); //$NON-NLS-1$
		String s = p.getProperty("expandingLevel"); //$NON-NLS-1$
		if(s != null && s.length() > 0) try {
			expandingLevel = Integer.parseInt(s);
		} catch (NumberFormatException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		makeObject();
	}
	
	void makeObject() {
		object = new CheckObject(null, new String[]{"", "no"}); //$NON-NLS-1$
		provider.setObject(object);
		for (int i = 0; i < vs.length; i++) {
			String path = vs[i][0];
			CheckObject c = object.getByPath(path);
			if(c != null) {
				c.objectData = vs[i];
			} else {
				c = create(vs[i]);
			}
		}
	}
	
	private CheckObject create(String[] data) {
		String path = data[0];
		int s = path.lastIndexOf('/');
		String parentPath = path.substring(0, s);
		CheckObject p = parentPath.length() == 0 ? object : object.getByPath(parentPath);
		if(p == null) p = create(new String[]{parentPath, "no"});
		CheckObject c = new CheckObject(p, data);
		p.addChild(c);
		return c;
	}
	
	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);
		
		treeViewer = new TreeViewer(composite, SWT.CHECK | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		treeViewer.setContentProvider(provider);
		treeViewer.setLabelProvider(provider);
		treeViewer.setInput(this);
		
		Object[] os = provider.getChildren(this);
		
		Control tc = treeViewer.getControl();
		tc.setLayoutData(new GridData(GridData.FILL_BOTH));
		initTree();
		treeSelectionManager = new TreeItemSelectionManager(treeViewer, new Flipper());
		treeViewer.expandToLevel(expandingLevel);
		Control bc = allBar.createControl(composite);
		bc.setLayoutData(new GridData(GridData.FILL_VERTICAL));
		return composite;
	}
	
	private void initTree() {
		Tree tree = treeViewer.getTree();
		TreeItem[] is = tree.getItems();
		for (int i = 0; i < is.length; i++) {
			Object d = is[i].getData();
			if(d instanceof CheckObject) {
				CheckObject o = (CheckObject)d;
				is[i].setChecked(!o.isDisabled());
			}
		}		
	}
	
	public void action(String command) {
		if(CANCEL.equals(command) ||
		   OK.equals(command) ||
		   HELP.equals(command)) {
		   	super.action(command);
		} else { 
			stopEditing();
			internalAction(command);
		}
	}

	protected abstract void internalAction(String command);
	
	protected void enableAll() {
		enableHierarchy(object);
		treeSelectionManager.update();
		treeViewer.refresh();
	}
	
	private void enableHierarchy(CheckObject o) {
		CheckObject[] os = o.getChildren();
		for (int i = 0; i < os.length; i++) {
			os[i].setEnabled(true);
			enableHierarchy(os[i]);
		}
	}

	protected void disableAll() {
		CheckObject[] os = object.getChildren();
		for (int i = 0; i < os.length; i++) {
			os[i].setEnabled(false);
		}
		treeSelectionManager.update();
		treeViewer.refresh();
	}
	
	class Flipper implements TreeItemSelectionManager.Listener {
		public void flip(TreeItem item) {
			if(item == null) return;
			CheckObject w = (CheckObject)item.getData();
			w.flip();
			treeViewer.refresh(w);
		}

		public boolean isSelected(Object data) {
			if(data instanceof CheckObject) {
				CheckObject o = (CheckObject)data;
				return !o.isDisabled();
			}
			return false;
		}		
	}
	
}

class CheckObject {
	boolean checked;
	String[] objectData;
	CheckObject parent;
	CheckObject[] children = new CheckObject[0];
	
	public CheckObject(CheckObject parent, String[] objectData) {
		this.parent = parent;
		this.objectData = objectData;
	}
	
	public void addChild(CheckObject child) {
		CheckObject[] cs = new CheckObject[children.length + 1];
		System.arraycopy(children, 0, cs, 0, children.length);
		cs[children.length] = child;
		children = cs;
	}
	
	public CheckObject getParent() {
		return parent;
	}
	
	public CheckObject[] getChildren() {
		return children;
	}
	
	public boolean isDisabled() {
		return "yes".equals(objectData[1]);
	}
	
	public boolean isNotDisabled() {
		return parent == null || (!parent.isDisabled() && parent.isNotDisabled());
	}
	
	public void flip() {
		objectData[1] = isDisabled() ? "no" : "yes";
	}
	
	public void setEnabled(boolean b) {
		objectData[1] = b ? "no" : "yes";
	}
	
	public String getPath() {
		return objectData[0];
	}
	
	public CheckObject getByPath(String path) {
		if(!path.startsWith(getPath() + "/")) return null;
		for (int i = 0; i < children.length; i++) {
			if(children[i].getPath().equals(path)) return children[i];
			CheckObject c = children[i].getByPath(path);
			if(c != null) return c;
		}
		return null;
	}
	
	public String toString() {
		String n = objectData[0];
		int i = n.lastIndexOf('/');
		return n.substring(i + 1);
	}

}

class CheckTreeProvider extends LabelProvider implements ITreeContentProvider, ILabelProvider, IColorProvider {
	public Image IMAGE_ENABLED = EclipseResourceUtil.getImage("images/common/check.gif"); //$NON-NLS-1$
	public Image IMAGE_DISABLED = EclipseResourceUtil.getImage("images/common/uncheck.gif"); //$NON-NLS-1$
	protected CheckObject object = null;
	
	public CheckObject getObject() {
		return object;
	}

	public void setObject(CheckObject object) {
		this.object = object;
	}

	public Object[] getChildren(Object parentElement) {
		return (parentElement instanceof CheckObject) ? ((CheckObject)parentElement).children : new CheckObject[0];
	}

	public Object getParent(Object element) {
		return (element instanceof CheckObject) ? ((CheckObject)element).getParent() : null;
	}

	public boolean hasChildren(Object element) {
		return (element instanceof CheckObject) && ((CheckObject)element).children.length > 0;
	}

	public Object[] getElements(Object inputElement) {
		return object.getChildren();
	}

	public void dispose() {}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {}

	/**
	 * Tree style SWT.CHECK is used instead.
	public Image getImage(Object element) {
		if(!(element instanceof CheckObject)) return null;
		CheckObject w = (CheckObject)element;
		return (!w.isDisabled()) ? IMAGE_ENABLED : IMAGE_DISABLED;
	}
	*/

	public Color getForeground(Object element) {
		if(!(element instanceof CheckObject)) return null;
		CheckObject w = (CheckObject)element;
		return (w.isNotDisabled()) ? null : Display.getDefault().getSystemColor(SWT.COLOR_GRAY);
	}

	public Color getBackground(Object element) {
		return null;
	}
	
}
