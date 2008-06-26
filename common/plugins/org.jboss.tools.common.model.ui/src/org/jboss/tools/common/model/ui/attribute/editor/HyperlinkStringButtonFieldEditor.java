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
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.actions.IActionProvider;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.xpl.SelectableFormLabel;

public class HyperlinkStringButtonFieldEditor extends StringButtonFieldEditor implements IFieldEditor, IPropertyChangeListener, PropertyChangeListener, IPropertyFieldEditor {
	protected PropertyEditorDialog editorDialog;
	protected IPropertyEditor propertyEditor; 
	protected IValueProvider valueProvider;
	protected IValueChangeListener valueChangeListener;
	protected Composite composite;
	private Label label;
	private IActionProvider actionProvider;
	private IAction labelAction;
	

	public HyperlinkStringButtonFieldEditor() {}
	
	public HyperlinkStringButtonFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected Label getHyperlinkLabelControl() {
		return label;
	}

	public Label getHyperlinkLabelControl(Composite parent) {
		if (label == null) { // cannot comment this! for details see label.addDisposeListener
			int style;
			Color bg;
			Color fg;
			Font font;

			if (this.labelAction!=null) {
				// hyperlink style
				style = getSettings().getStyle("Hyperlink.Style");
				if (style==SWT.DEFAULT) style = SWT.NONE;
				bg = parent.getBackground();
					///getSettings().getColor("Hyperlink.Background");
				fg = getSettings().getColor("Hyperlink.Foreground");
				font = getSettings().getFont("Hyperlink.Font");
				label = new SelectableFormLabel(parent, style);
				((SelectableFormLabel)label).addSelectionListener(new SelectionListener() {
					public void widgetSelected(SelectionEvent e) {
						labelAction.run();
					}
					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});
			} else {
				// label
				style = getSettings().getStyle("Label.Style");
				bg = parent.getBackground();
					///getSettings().getColor("Label.Background");
				fg = getSettings().getColor("Label.Foreground");
				font = getSettings().getFont("Label.Font");
				label = new Label(parent, style);
			}
		
			label.setFont(font);
///			label.setBackground(bg);
			label.setForeground(fg);
			label.setEnabled(isEnabled());
			String text = getLabelText();
			if (text != null)
				label.setText(text);
			label.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					label = null;
				}
			});
		} else {
			checkParent(label, parent);
		}
		return label;
	}



	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
		valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
		actionProvider = (IActionProvider)propertyEditor.getAdapter(IActionProvider.class);
		if (actionProvider!=null) {
			labelAction = actionProvider.getAction("LABEL_ACTION");
		}
		setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
	}

	protected String changePressed() {
		editorDialog = new PropertyEditorDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), propertyEditor);
		int i = editorDialog.open();
		return (i != PropertyEditorDialog.OK) ? null : valueProvider.getStringValue(false);
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		Control control = getTextChangeControl(parent);
		control.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	protected Composite getTextChangeControl(Composite parent)
	{
		//if (composite == null) 
		createTextChangeControl(parent);
		return composite;
	}
	
	protected Control createTextChangeControl(Composite parent) {
		GridData gd;
		Control control;
		composite = new Composite(parent, SWT.NONE);
		composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
///		composite.setBackground(parent.getBackground());
		GridLayout gridLayout = new GridLayout(3, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		
		Control textControl = createTextControl(composite);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		textControl.setLayoutData(gd);

		control = new Label(composite, SWT.NONE);
///		control.setBackground(parent.getBackground());
		gd = new GridData();
		gd.widthHint = 5;
		control.setLayoutData(gd);
		
		control = getChangeControl(composite);
		gd = new GridData();
		gd.widthHint = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
		gd.heightHint = textControl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);
		
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
		if ((label != null) && (!label.isDisposed())) {
			label.setEnabled(enabled);
		}
	}
	
	// IFieldEditor
	public Control[] getControls(Composite parent) 
	{
		return new Control[] {getHyperlinkLabelControl(parent), getTextChangeControl(parent)};
/*		
		Control mainPanelChildren[] = getTextChangeControl(parent).getChildren(); 
		Control result[] = new Control[mainPanelChildren.length + 1];
		result[0] = getLabelComposite(parent);
		System.arraycopy(mainPanelChildren, 0, result, 1, mainPanelChildren.length);
		return result;
*/		
	}
	
	
}
