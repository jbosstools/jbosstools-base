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
import java.io.File;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class DirectoryFieldEditorEx extends StringButtonFieldEditorEx implements IPropertyFieldEditor, IFieldEditor, IPropertyChangeListener, PropertyChangeListener {

	protected String dirChooserLabelText = null;
	protected String lastPath = null;
	
	protected IPropertyEditor propertyEditor;

	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider; 
	
	private Composite textChangeControl;

	public DirectoryFieldEditorEx() {}

	public DirectoryFieldEditorEx(IWidgetSettings settings) {
		super(settings);
	}

	protected void init() {
		setStringValue(valueProvider.getStringValue(true));
		setPropertyChangeListener(this);
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		
		GridData gd;
		Control control = createTextChangeControl(parent);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		control.setLayoutData(gd);
	}

	protected Control createTextChangeControl(Composite parent) {
		GridData gd;
		Control control;
		if (textChangeControl == null) {
			textChangeControl = new Composite(parent, SWT.NONE);
			GridLayout gridLayout = new GridLayout(3, false);
			gridLayout.marginHeight = 0;
			gridLayout.marginWidth = 0;
			gridLayout.horizontalSpacing = 0;
			gridLayout.verticalSpacing = 0;
			textChangeControl.setLayout(gridLayout);
		
			Control textControl = createTextControl(textChangeControl);
			gd = new GridData(GridData.FILL_HORIZONTAL);
			textControl.setLayoutData(gd);

			control = new Label(textChangeControl, SWT.NONE);
			gd = new GridData();
			gd.widthHint = 5;
			control.setLayoutData(gd);
		
			control = getChangeControl(textChangeControl);
			gd = new GridData();
			gd.widthHint = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
			gd.heightHint = textControl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
			control.setLayoutData(gd);

			// init
			init();
		}

		return textChangeControl;
	}
	
	protected String changePressed() {
		Object v = callExternal(getShell());
		return (v == null) ? null : v.toString();
	}

	public Object callExternal(Shell shell) {
		DirectoryDialog dialog = new DirectoryDialog(shell);
		if (dirChooserLabelText != null)
			dialog.setMessage(dirChooserLabelText);
		if(propertyEditor != null && propertyEditor.getValue() != null && propertyEditor.getValue().toString().trim().length() > 0) {
			String v = "" + propertyEditor.getValue();
			if(new File(v).isFile()) v = new File(v).getParent(); 
			dialog.setFilterPath(v);
		} else if (lastPath != null) {
			if (new File(lastPath).exists())
				dialog.setFilterPath(lastPath);
		}
		String dir = dialog.open();
		if (dir != null) {
			dir = dir.trim();
			if (dir.length() == 0)
				return null;
			lastPath = dir;
		}
		return dir;
	}
	
	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTextChangeControl(parent)};
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
		valueProvider.addValueChangeListener(this);
		init();
		this.dirChooserLabelText = propertyEditor.getLabelText();
		String v = valueProvider.getStringValue(false);
		if(v != null && v.length() > 0 && new File(v).isDirectory()) this.lastPath = v;
		this.setPropertyChangeListener(this);
		setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
	}

	// IPropertyChangeListener
	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
		if (valueChangeListener!=null) {
			if (FieldEditor.VALUE.equals(event.getProperty())) {
				setPropertyChangeListener(null);
				java.beans.PropertyChangeEvent e = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, event.getOldValue(), event.getNewValue());
				valueChangeListener.valueChange(e);
				setPropertyChangeListener(this);
			}
		}
	}
	
	// PropertyChangeListener
	public void propertyChange(PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			this.setStringValue((v == null) ? "" : v.toString());
		}
		valueProvider.addValueChangeListener(this);
	}

	public int getNumberOfControls() {
		return 2;
	}

	protected void checkParent(Control control, Composite parent) {
	}
	
	public String getLastPath() {
		return lastPath;
	}
	
	public void setLastPath(String lastPath) {
		this.lastPath = lastPath;
	}
	
}
