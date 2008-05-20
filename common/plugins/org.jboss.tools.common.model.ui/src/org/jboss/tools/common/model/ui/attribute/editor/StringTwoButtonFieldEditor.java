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

import java.beans.PropertyChangeListener;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class StringTwoButtonFieldEditor extends StringButtonFieldEditor implements IFieldEditor, IPropertyChangeListener, PropertyChangeListener, IPropertyFieldEditor {
	protected PropertyEditorDialog editorDialog;
	protected IPropertyEditor propertyEditor; 
	protected IValueProvider valueProvider;
	protected IValueChangeListener valueChangeListener;
	protected Composite composite;
	
	protected String[] buttonLabels;
	protected Button button1;
	protected Button button2;

	public StringTwoButtonFieldEditor() {}
	
	public StringTwoButtonFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
		valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
		setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
	}

	protected String changePressed() {
		editorDialog = new PropertyEditorDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), propertyEditor);
		int i = editorDialog.open();
		return (i != PropertyEditorDialog.OK) ? null : valueProvider.getStringValue(false);
	}

	protected String change2Pressed() {
		return valueProvider.getStringValue(false);
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		Control control = getTextChangeControl(parent);
		control.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	protected Composite getTextChangeControl(Composite parent)
	{
		if (composite == null) createTextChangeControl(parent);
		return composite;
	}
	
	protected Control getChange1Control(Composite parent) {
		button1 = this.getChangeControl(parent); 
		// openBrowse=Browse
		// open=Open
		String buttonLabel;
		if ((this.buttonLabels!=null)&&(buttonLabels.length>1)) {
			buttonLabel = buttonLabels[0];
		} else {
			buttonLabel = JFaceResources.getString("openBrowse");
		}
		button1.setText(buttonLabel);
		return button1;
	}
	
	protected Control getChange2Control(Composite parent) {
		if (button2 == null) {
			int style = getSettings().getStyle("Button.Style");
			if (style==SWT.DEFAULT) style = SWT.NONE;
			if (style==0) style = SWT.PUSH;
			Color bg = getSettings().getColor("Button.Background");
			Color fg = getSettings().getColor("Button.Foreground");
			Font font = getSettings().getFont("Button.Font");
			button2 = new Button(parent, style);
			button2.setFont(font);
			button2.setBackground(bg);
			button2.setBackground(bg);
			button2.setForeground(fg);

			// openBrowse=Browse
			// open=Open
			String buttonLabel;
			if ((this.buttonLabels!=null)&&(buttonLabels.length>1)) {
				buttonLabel = buttonLabels[1];
			} else {
				buttonLabel = JFaceResources.getString("open");
			}
			button2.setText(buttonLabel);
			button2.setFont(parent.getFont());
			button2.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent evt) {
					String newValue = change2Pressed();
					if (newValue != null) {
						setStringValue(newValue);
					}
				}
			});
			button2.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					button2 = null;
				}
			});
		} else {
			checkParent(button2, parent);
		}
		return button2;
	}
	
	protected Control createTextChangeControl(Composite parent) {
		GridData gd;
		Control control;
		composite = new Composite(parent, SWT.NONE);
		composite.setBackground(parent.getBackground());
		GridLayout gridLayout = new GridLayout(5, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);

		// text control		
		Control textControl = createTextControl(composite);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		textControl.setLayoutData(gd);

		// separator
		control = new Composite(composite, SWT.NONE);
		control.setBackground(parent.getBackground());
		gd = new GridData();
		gd.widthHint = 5;
		gd.heightHint = 5;
		control.setLayoutData(gd);
		
		// button1 control		
		control = getChange1Control(composite);
		gd = new GridData();
		//gd.widthHint = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
		gd.heightHint = textControl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);
		
		// separator
		control = new Composite(composite, SWT.NONE);
		control.setBackground(parent.getBackground());
		gd = new GridData();
		gd.widthHint = 5;
		gd.heightHint = 5;
		control.setLayoutData(gd);
		
		// button2 control		
		control = getChange2Control(composite);
		gd = new GridData();
		//gd.widthHint = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
		gd.heightHint = textControl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);
		
		// initialize
		if (this.valueProvider!=null) {
			getTextField().setText(""+valueProvider.getValue());
		}
		
		return composite;
	}

	// IPropertyChangeListener
	public void propertyChange(PropertyChangeEvent event) {
		if (valueChangeListener!=null) {
			if (ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
				setPropertyChangeListener(null);
				Object oldValue = event.getOldValue();
				Object newValue = event.getNewValue();
				java.beans.PropertyChangeEvent e = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
				valueChangeListener.valueChange(e);
				setPropertyChangeListener(this);
			}
		}
	}

	public void propertyChange(java.beans.PropertyChangeEvent evt) {
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			valueProvider.removeValueChangeListener(this);
			this.setStringValue((v == null) ? "" : v.toString());
			valueProvider.addValueChangeListener(this);
		}
	}

	public int getNumberOfControls() {
		return 2;
	}

	public void setEnabled(boolean enabled, Composite parent) {
		Control controls[] = getControls(parent);
		for (int i = 0; i < controls.length; i++) {
			if (controls[i] instanceof Composite) {
				Control compositeChildren[] = ((Composite)controls[i]).getChildren();
				for (int j = 0; j < compositeChildren.length; compositeChildren[j++].setEnabled(enabled));
			}
			controls[i].setEnabled(enabled); 
		}
	}
	
	// IFieldEditor
	public Control[] getControls(Composite parent) 
	{
		return new Control[] {getLabelComposite(parent), getTextChangeControl(parent)};
/*		
		Control mainPanelChildren[] = getTextChangeControl(parent).getChildren(); 
		Control result[] = new Control[mainPanelChildren.length + 1];
		result[0] = getLabelComposite(parent);
		System.arraycopy(mainPanelChildren, 0, result, 1, mainPanelChildren.length);
		return result;
*/		
	}
	
	
	/**
	 * @return
	 */
	public String[] getButtonLabels() {
		return buttonLabels;
	}

	/**
	 * @param strings
	 */
	public void setButtonLabels(String[] strings) {
		buttonLabels = strings;
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (getTextControl()!=null) {
			getTextControl().setEnabled(enabled);
		}
		if (this.button1!=null) {
			this.button1.setEnabled(enabled);
		}
		if (this.button2!=null) {
			this.button2.setEnabled(enabled);
		}
	}
}
