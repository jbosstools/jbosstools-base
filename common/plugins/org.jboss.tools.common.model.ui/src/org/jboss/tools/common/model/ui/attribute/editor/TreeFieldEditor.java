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
package org.jboss.tools.common.model.ui.attribute.editor;

import org.jboss.tools.common.model.ui.attribute.IPropertyDescriptorEx;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class TreeFieldEditor extends ExtendedFieldEditor {
	
	protected IPropertyDescriptorEx description; 

	private TreeViewer tree = null;
	private Text text; 
	
	private String stringValue;
	
	private Composite treeAndText;
	
	public TreeFieldEditor() {}

	public TreeFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	public TreeFieldEditor(String name, String labelText, Composite parent) {
		init(name, labelText);
		createControl(parent);
	}

	protected void adjustForNumColumns(int numColumns) {
		Control control = getLabelComposite();
		((GridData)control.getLayoutData()).horizontalSpan = numColumns;
		((GridData)treeAndText.getLayoutData()).horizontalSpan = numColumns - 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelComposite(parent);
		GridData gd = new GridData();
		//gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
		gd.verticalAlignment = GridData.GRAB_VERTICAL;
		control.setLayoutData(gd);

		treeAndText = getTreeAndTextControl(parent);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.verticalAlignment = GridData.FILL;
		gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = true;
		treeAndText.setLayoutData(gd);
	}

	protected void doLoad() {
		if (treeAndText != null) {
//			String s = getPreferenceStore().getString(getPreferenceName());
			// TODO:
		}
	}

	protected void doLoadDefault() {
		if (treeAndText != null) {
//			String s = getPreferenceStore().getDefaultString(getPreferenceName());
			// TODO:
		}
	}

	protected void doStore() {
		if (treeAndText != null) {
			// TODO:
		}
	}

	public int getNumberOfControls() {
		return 2;
	}

	protected Control getTreeControl() {
		if (tree!=null) return tree.getControl();
		return null;
	}
	
	protected Control getTextControl() {
		return text;
	}
	
	protected Control createTreeControl(Composite parent) {
		if (this.tree==null) {
			tree = new TreeViewer(parent, SWT.BORDER);
		}
		return tree.getControl();
	}
	
	protected Control createTextControl(Composite parent) {
		if (this.text==null) {
			text = new Text(parent, SWT.SINGLE | SWT.BORDER);
		}
		return text;
	}

	protected Composite getTreeAndTextControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1,false);
		composite.setLayout(layout);
		
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		
		GridData gd;
		Control control;
		
		control = createTreeControl(composite);
		gd = new GridData(GridData.FILL_BOTH);
		control.setLayoutData(gd);
		
		control = createTextControl(composite);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		control.setLayoutData(gd);
		
		return composite;
	}

	public String getStringValue() {
		return stringValue;
	}

	public void setStringValue(String string) {
		stringValue = string;
	}

	public IPropertyDescriptorEx getDescription() {
		return description;
	}

	public void setDescription(IPropertyDescriptorEx description) {
		this.description = description;
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (getTreeControl()!=null) getTreeControl().setEnabled(enabled);
		if (getTextControl()!=null) getTextControl().setEnabled(enabled);
	}

	public void cut() {
	}

	public void copy() {
	}

	public void paste() {
	}

	public void delete() {
	}

}
