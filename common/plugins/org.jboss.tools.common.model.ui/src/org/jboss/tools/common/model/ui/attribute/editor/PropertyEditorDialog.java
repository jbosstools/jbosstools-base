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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.DefaultSettings;

public class PropertyEditorDialog extends Dialog {
	public static final int MINIMUM_WIDTH = 400;
	public static final int MINIMUM_HEIGHT = 300;
	IPropertyEditor propertyEditor;
	ExtendedFieldEditor editor;
	DefaultValueAdapter adapter;
	PropertyChangeListener listener = new VCL();
	Object initValue;
	
	public PropertyEditorDialog(Shell parentShell, IPropertyEditor editor) {
		super(parentShell);
		this.propertyEditor = editor;
		initValue = editor.getValue();
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite)super.createDialogArea(parent);
		Layout pageContainerLayout = new LayoutFactory(parent.getShell()).createLayout(5, 5, MINIMUM_WIDTH, MINIMUM_HEIGHT);
		//GridLayout pageContainerLayout = new GridLayout();
		composite.setLayout(pageContainerLayout);
		createDialogAreaInternal(composite);
		if(propertyEditor.getInput() instanceof DefaultValueAdapter) {
			adapter = (DefaultValueAdapter)propertyEditor.getInput();
			adapter.addValueChangeListener(listener);
		}
		return composite;
	}

	protected Control createButtonBar(Composite parent) {
		Control c = super.createButtonBar(parent);
		Button b = getButton(IDialogConstants.OK_ID);
		b.setEnabled(false);
		return c;
	}

	protected Control createDialogAreaInternal(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);		
		createFieldEditor(composite);		
		return composite;
	}

	protected void createFieldEditor(Composite parent) {
		editor = propertyEditor.getFieldEditor(parent);
		editor.setSettings(new DefaultSettings());
		
		int cn = editor.getNumberOfControls();
		if(cn > 2) {
			GridLayout layout = new GridLayout();
			layout.numColumns = cn;
			parent.setLayout(layout);
		} else cn = 2;
		editor.fillIntoGrid(parent, cn);
		editor.setOwnerDialog(this);
	}

	public void okPressed() {
		Button b = getButton(IDialogConstants.OK_ID);
		if(b == null || !b.isEnabled()) {
			return;
		}
		super.okPressed();
	}

	protected void buttonPressed(int buttonId) {
		if (IDialogConstants.OK_ID == buttonId) {
			editor.store();
		}
		super.buttonPressed(buttonId);
	}
	
	public void dispose() {
		if(adapter != null) {
			adapter.removeValueChangeListener(listener);
			adapter = null;
		}
		if(editor != null) editor.setOwnerDialog(null);
	}
	
	class VCL implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent event) {
			Object value = event.getNewValue();
			Button b = getButton(IDialogConstants.OK_ID);
			if(b == null || b.isDisposed()) {
				dispose();
				return;
			}
			boolean enabled = (value != null && !value.equals(initValue));
			b.setEnabled(enabled);
		}

	}
	
	class LayoutFactory extends WizardDialog {
		public LayoutFactory(Shell shell) {
			super(shell, null);
		}
	    protected void setWizard(IWizard newWizard) {
	    }
	    public Layout createLayout(int mw, int mh, int minW, int minH) {
	    	return new PageContainerFillLayout(mw, mh, minW, minH) {
	    	    public Point computeSize(Composite composite, int wHint, int hHint,
	    				boolean force) {
	    	    	Point p = super.computeSize(composite, wHint, hHint, force);
	    	    	if(p.y > 400) p.y = 400;
	    	    	return p;
	    	    }
	    		
	    	};
	    
	    }
		
	}
}
