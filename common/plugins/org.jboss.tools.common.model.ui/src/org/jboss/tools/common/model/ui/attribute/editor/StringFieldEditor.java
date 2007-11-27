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

import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.ScrolledComposite;
import org.jboss.tools.common.model.ui.widgets.border.Border;
 
public class StringFieldEditor extends ExtendedFieldEditor {

	public static final int VALIDATE_ON_KEY_STROKE = 0;
	public static final int VALIDATE_ON_FOCUS_LOST = 1;
	public static int UNLIMITED = -1;
	private boolean isValid;
	private String oldValue;
	private Text textField; // real text control
	private Control textControl; // maybe border control with text control inside
	private int charCapacity = UNLIMITED;
	private int textLimit = UNLIMITED;
	private String error;
	private boolean emptyStringAllowed = true;
	private int validateStrategy= VALIDATE_ON_KEY_STROKE;
	
	public StringFieldEditor() {}

	public StringFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)textField.getLayoutData();
		gd.horizontalSpan = numColumns - 1;
		// We only grab excess space if we have to
		// If another field editor has more columns then
		// we assume it is setting the width.
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
	}

	protected boolean validate() {
		boolean result = false;
		if (emptyStringAllowed) result = true;
		if (textField == null) result = false;
		String txt = textField.getText();
		if (txt == null) {
			result = false;
		} else {
			result = (txt.trim().length() > 0) || emptyStringAllowed;
		}
		result = result && doValidate();
		if (result)
			clearErrorMessage();
		else
			showErrorMessage(error);
		return result;
	}

	protected boolean doValidate() {
		return true;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
	
		createTextControl(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns - 1;
		if (charCapacity != UNLIMITED) {
			GC gc = new GC(textField);
			try {
				Point extent = gc.textExtent("X");//$NON-NLS-1$
				gd.widthHint = charCapacity * extent.x;
			} finally {
				gc.dispose();
			}
		} else {
			gd.horizontalAlignment = GridData.FILL;
			gd.grabExcessHorizontalSpace = true;
		}
		textField.setLayoutData(gd);
	}

	protected void doLoad() {
		throw new RuntimeException("Not implemented");
	}

	protected void doLoadDefault() {
		throw new RuntimeException("Not implemented");
	}

	protected void doStore() {
		getPreferenceStore().setValue(getPreferenceName(), textField.getText());
	}

	public int getNumberOfControls() {
		return 2;
	}

	public String getStringValue() {
		if (textField != null)
			return textField.getText();
		else
			return getPreferenceStore().getString(getPreferenceName());
	}

	protected Text getTextField() {
		return textField;
	}
	
	protected Control getTextControl() {
		return textControl;
	}
	
	public Control createTextControl(Composite parent) {
		///do not restore this assignment! glory
		///textControl = textField;
		if (textField == null) {
			int style = getSettings().getStyle("Text.Style");
			if (style==SWT.DEFAULT) style = SWT.NONE;
///			Color bg = getSettings().getColor("Text.Background");
			Color fg = getSettings().getColor("Text.Foreground");
			Font font = getSettings().getFont("Text.Font");
			Border border = getSettings().getBorder("Text.Border");
			boolean readOnly = isAlwaysReadOnly();
			if (border != null) {
				if(readOnly) style |= SWT.READ_ONLY;
				BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, border);
				textField = new Text(borderedControl, style);
				textControl = borderedControl;
			} else {
				int style2 = SWT.BORDER;
				if(readOnly) style2 |= SWT.READ_ONLY;
				textField = new Text(parent, style2);
				textControl = textField;
			}
			textField.setFont(font);
///			textField.setBackground(bg);
			textField.setForeground(fg);

			switch (validateStrategy) {
				case VALIDATE_ON_KEY_STROKE :
					textField.addModifyListener(new ModifyListener() {
						public void modifyText(ModifyEvent e) {
							valueChanged();
						}						
					});
	
					textField.addFocusListener(new FocusAdapter() {
						public void focusGained(FocusEvent e) {
							refreshValidState();
						}
						public void focusLost(FocusEvent e) {
							clearErrorMessage();
						}
					});
					break;
				case VALIDATE_ON_FOCUS_LOST :
					textField.addKeyListener(new KeyAdapter() {
						public void keyPressed(KeyEvent e) {
							clearErrorMessage();
						}
					});
					textField.addFocusListener(new FocusAdapter() {
						public void focusGained(FocusEvent e) {
							refreshValidState();
						}
						public void focusLost(FocusEvent e) {
							valueChanged();
							clearErrorMessage();
						}
					});
					break;
				default :
					Assert.isTrue(false, "Unknown validate strategy");//$NON-NLS-1$
			}
			textField.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					textField = null;
				}
			});
			if(textLimit > 0){//Only set limits above 0 - see SWT spec
				textField.setTextLimit(textLimit);
			}
		} else {
			checkParent(textControl, parent);
		}
		//SampleContentAssistentProcessor �ontentProcessor = new SampleContentAssistentProcessor();
		//ControlContentAssistHelper.createTextContentAssistant(getTextField(), �ontentProcessor);
		textField.addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent e) {
				if(textField == null || textField.isDisposed()) return;
				ScrolledComposite.scrollToVisible(textField, new Rectangle(0, 0, textField.getBounds().width, textField.getBounds().height));
			}
			public void focusLost(FocusEvent e) {
			}
		});
		return textControl;
	}
	
	/**
	 * We cannot controls to provide READ-ONLY property on the fly.
	 * Instead, let us at least create it as READ-ONLY when it has
	 * no chance to get editable, for instance, if object to edit 
	 * is from jar.
	 * @return
	 */
	protected boolean isAlwaysReadOnly() {
		return false;
	}
	
	public boolean isEmptyStringAllowed() {
		return emptyStringAllowed;
	}

	public boolean isValid() {
		return isValid;
	}

	protected void refreshValidState() {
		isValid = validate();
	}

	public void setEmptyStringAllowed(boolean b) {
		emptyStringAllowed = b;
	}

	public void setErrorMessage(String message) {
		error = message;
	}

	public void setFocus() {
		if (textField != null) {
			textField.setSelection(0, textField.getText().length());
			textField.setFocus();
		}
	}

	public void setStringValue(String value) {
		if (textField != null) {
			if (value == null)
				value = "";//$NON-NLS-1$
			oldValue = textField.getText();
			if (!oldValue.equals(value)) {
				String ov = oldValue;
				Point p = textField.getSelection();
				textField.setText(value);
				int i = findPosition(ov, value, p.x);
				textField.setSelection(i);
				valueChanged();
			}
		}
	}
	
	private int findPosition(String ov, String nv, int p) {
		if(p == ov.length() || p < 0) return nv.length();
		for (int i = 0; i < p; i++) {
			if(i >= ov.length() || i >= nv.length()) return nv.length();
			if(ov.charAt(i) != nv.charAt(i)) return i;			
		}
		return p;		
	}

	public void setTextLimit(int limit) {
		textLimit = limit;
		if (textField != null)
			textField.setTextLimit(limit);
	}

	public void setValidateStrategy(int value) {
		Assert.isTrue(value == VALIDATE_ON_FOCUS_LOST || value == VALIDATE_ON_KEY_STROKE);
		validateStrategy = value;
	}

	protected void valueChanged() {
		setPresentsDefaultValue(false);
		boolean oldState = isValid;
		refreshValidState();
	
		if (isValid != oldState)
			fireStateChanged(IS_VALID, oldState, isValid);
	
		String newValue = textField.getText();
		if (!newValue.equals(oldValue)) {
			fireValueChanged(VALUE, oldValue, newValue);
			oldValue = textField.getText();
		}
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		setTextControlEnabled(enabled);
		updateErrorState();
	}
	
	protected void setTextControlEnabled(boolean enabled) {
		Control c = getTextControl();
		if (c != null && !c.isDisposed()) {
			Text text = getTextField();
			if(text != null && !text.isDisposed()) {
				text.setEditable(enabled);
			} else {
				boolean e = isAlwaysReadOnly();
				c.setEnabled(enabled || e);
			}			
		}
	}
	
	public void cut() {
		if (textField != null 
				&& textField.isFocusControl()
				&& isEnabled()) {
			textField.cut();
			valueChanged();
		}
	}

	public void copy() {
		if (textField != null && textField.isFocusControl()) {
			textField.copy();
		}
	}

	public void paste() {
		if (textField != null 
				&& textField.isFocusControl()
				&& isEnabled()) {
			textField.paste();
			valueChanged();
		}
	}

	public void delete() {
	}
}
