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

import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.util.IPropertyChangeListener;
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
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.ScrolledComposite;
import org.jboss.tools.common.model.ui.widgets.border.Border;

public class NoteFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	protected IPropertyEditor propertyEditor;
	
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;

	public static final int VALIDATE_ON_KEY_STROKE = 0;
	public static final int VALIDATE_ON_FOCUS_LOST = 1;
	public static int UNLIMITED = -1;
	private boolean isValid;
	protected String stringValue;
	private String oldValue;
	private Text textField;
	private int widthInChars = UNLIMITED;
	private int textLimit = UNLIMITED;
	private String errorMessage;
	private int validateStrategy = VALIDATE_ON_KEY_STROKE;

	public NoteFieldEditor() {}

	public NoteFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	public NoteFieldEditor(String name, String labelText, int width, int strategy, Composite parent) {
		init(name, labelText);
		widthInChars = width;
		setValidateStrategy(strategy);
		isValid = false;
		errorMessage = JFaceResources.getString("StringFieldEditor.errorMessage");//$NON-NLS-1$
		createControl(parent);
	}
	public NoteFieldEditor(String name, String labelText, int width, Composite parent) {
		this(name, labelText, width, VALIDATE_ON_KEY_STROKE, parent);
	}
	public NoteFieldEditor(String name, String labelText, Composite parent) {
		this(name, labelText, UNLIMITED, parent);
	}
	protected void adjustForNumColumns(int numColumns) {
		GridData d = (GridData)textField.getLayoutData();
		d.grabExcessHorizontalSpace = d.horizontalSpan == 1;
		d.horizontalSpan = numColumns - 1;
	}
	protected boolean checkState() {
		return true;
	}
	protected boolean doCheckState() {
		return true;
	}
	
	public void fillIntoGrid(Composite parent, int numColumns) {
		//Assert.isTrue(numColumns >= getNumberOfControls());
		Assert.isTrue(parent.getLayout() instanceof GridLayout);
		doFillIntoGrid(parent, numColumns);
	}
	
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelComposite(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		gd.verticalAlignment = GridData.GRAB_VERTICAL;
		control.setLayoutData(gd);

		textField = createTextControl(parent);
//		int height = textField.computeSize(-1,-1).y;
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;

		//if (height<60) {
		//	gd.heightHint = 60;
		//} else {
		//	gd.verticalAlignment = GridData.FILL_VERTICAL;
		//}
		
		if (widthInChars != UNLIMITED) {
			GC gc = new GC(textField);
			try {
				Point extent = gc.textExtent("X");//$NON-NLS-1$
				int ext = extent.x;
				if(ext > 30) ext = 30;
				gd.widthHint = widthInChars * ext;
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
		if (textField != null) {
			String value = getPreferenceStore().getString(getPreferenceName());
			textField.setText(value);
			oldValue = getTextFieldValue();
		}
	}

	protected void doLoadDefault() {
		if (textField != null) {
			String value = getPreferenceStore().getDefaultString(getPreferenceName());
			textField.setText(value);
		}
		valueChanged();
	}

	protected void doStore() {
		getPreferenceStore().setValue(getPreferenceName(), getTextFieldValue());
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public int getNumberOfControls() {
		return 2;
	}
	public String getStringValue() {
		if (textField != null)
			return getTextFieldValue();
		else if (this.stringValue!=null)
			return this.stringValue;
		else
			return getPreferenceStore().getString(getPreferenceName());
	}
	protected Text getTextControl() {
		return textField;
	}
	public Text createTextControl(Composite parent) {
		if (textField == null) {
			int style = getSettings().getStyle("Note.Style");
			if (style==SWT.DEFAULT) style = SWT.NONE;
			Color bg = getSettings().getColor("Note.Background");
			Color fg = getSettings().getColor("Note.Foreground");
			Font font = getSettings().getFont("Note.Font");
			Border border = getSettings().getBorder("Note.Border");
			this.validateStrategy = getSettings().getInt("Note.ValidateStrategy");
			if (validateStrategy==SWT.DEFAULT) validateStrategy = VALIDATE_ON_KEY_STROKE;
			boolean b = isAlwaysReadOnly();
			if (border != null) {
				if(b) style |= SWT.READ_ONLY;
				BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, border);
				textField = new Text(borderedControl, style);
			} else {
				int style2 = SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL;
				if(b) style2 |= SWT.READ_ONLY;
				textField = new Text(parent, style);
			}
			textField.getAccessible();
			if(propertyEditor.getInput() instanceof DefaultValueAdapter) {
				DefaultValueAdapter a = (DefaultValueAdapter)propertyEditor.getInput();
				String s = (a.getAttribute() == null) ? null : a.getAttribute().getProperty("font");
				if(s != null) {
					Font oldFont = textField.getFont();
					FontData data = oldFont.getFontData()[0];
					data.setName(s);
					try {
						font = new Font(null, data);
					} catch (Exception e) {
						ModelUIPlugin.log(e);
					}
				}
			}
			textField.setFont(font);
			textField.setBackground(bg);
			textField.setForeground(fg);
			//textField = new Text(parent, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
			//textField.setFont(parent.getFont());
			if (this.stringValue!=null){
				textField.setText(this.getStringValue()); 
			}
			switch (validateStrategy) {
				case VALIDATE_ON_KEY_STROKE :
					/*
					textField.addKeyListener(new KeyAdapter() {
						public void keyReleased(KeyEvent e) {
							valueChanged();
						}
					});
					*/
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
					textField.addModifyListener(new ModifyListener() {
						public void modifyText(ModifyEvent e) {
							if(textField == null || textField.isDisposed()) return;
							String newValue = getTextFieldValue();
							Object o = propertyEditor.getInput();
							if(o instanceof DefaultValueAdapter) {
								DefaultValueAdapter a = (DefaultValueAdapter)o;
								XModelObject object = a.getModelObject();
								if(object != null && object.isObjectEditable()) {
									String oldValue = object.getAttributeValue(a.getAttribute().getName());
									if(oldValue != null) oldValue = removeRSymbol(oldValue);
									if(oldValue != null && !oldValue.equals(newValue)) {
										object.setModified(true);
									}
								}
							}
						}						
					});
					break;
				default :
					Assert.isTrue(false, "Unknown validate strategy");//$NON-NLS-1$
			}
			textField.addFocusListener(new FocusListener() {
				public void focusGained(FocusEvent e) {
					if(textField == null || textField.isDisposed()) return;
					ScrolledComposite.scrollToVisible(textField, new Rectangle(0, 0, 100, textField.getBounds().height));
				}
				public void focusLost(FocusEvent e) {
				}
			});
			textField.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					textField = null;
				}
			});
			if(textLimit > 0){//Only set limits above 0 - see SWT spec
				textField.setTextLimit(textLimit);
			} else {
				textField.setTextLimit(Text.LIMIT);
			}
		} else {
			checkParent(textField, parent);
		}
		init();
		return textField;
	}
	public boolean isValid() {
		return isValid;
	}
	protected void refreshValidState() {
		isValid = checkState();
	}
	public void setErrorMessage(String message) {
		errorMessage = message;
	}
	public void setFocus() {
		if (textField != null) {
			textField.setFocus();
		}
	}
	public void setStringValue(String value) {
		this.stringValue = removeRSymbol(value);
		value = stringValue;
		if (textField != null) {
			if (value == null)
				value = "";//$NON-NLS-1$
			oldValue = getTextFieldValue();
			if (!oldValue.equals(value)) {
				textField.setText(value);
				valueChanged();
			}
		}
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
	public void showErrorMessage() {
		showErrorMessage(errorMessage);
	}
	protected void valueChanged() {
		setPresentsDefaultValue(false);
		boolean oldState = isValid;
		refreshValidState();

		if (isValid != oldState)
			fireStateChanged(IS_VALID, oldState, isValid);

		String newValue = getTextFieldValue();
		if (!newValue.equals(oldValue)) {
			fireValueChanged(VALUE, oldValue, newValue);
			PropertyChangeEvent event = new PropertyChangeEvent(this, "value", oldValue, newValue);
			valueChangeListener.valueChange(event);
			oldValue = newValue;
		}
	}
	
	private String getTextFieldValue() {
		String v = textField.getText();
		return removeRSymbol(v);
	}
	
	private String removeRSymbol(String v) {
		if(v == null) return null;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < v.length(); i++) {
			char c = v.charAt(i);
			if(c != '\r' || (i + 1 < v.length() && v.charAt(i + 1) != '\n')) {
				sb.append(c);
			}
		}
		return sb.toString();
	}

	// IValueEditor
	public void setValueChangeListener(IValueChangeListener valueChangeListener) {
		this.valueChangeListener = valueChangeListener;
	}
	public void setValueProvider(IValueProvider valueProvider) {
		this.valueProvider = valueProvider;
	}

	protected void init() {
		setStringValue(valueProvider.getStringValue(true));
		setPropertyChangeListener(this);
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
		init();
		valueProvider.addValueChangeListener(this);
	}

	// IPropertyChangeListener
	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			PropertyChangeEvent e = new PropertyChangeEvent(this, IPropertyEditor.VALUE, event.getOldValue(), event.getNewValue());
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTextControl(parent)};
	}

	public void propertyChange(PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			setStringValue(valueProvider.getStringValue(true));
		}
		valueProvider.addValueChangeListener(this);
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if(textField != null && !textField.isDisposed()) {
			textField.setEditable(enabled);
		} else if (getTextControl() != null && !getTextControl().isDisposed()) {
			boolean e = isAlwaysReadOnly();
			getTextControl().setEnabled(enabled || e);
		}			
	}

	public void cut() {
		if (this.textField!=null && this.textField.isFocusControl()) {
			this.textField.cut();
			this.valueChanged();
		}
	}

	public void copy() {
		if (this.textField!=null && this.textField.isFocusControl()) {
			this.textField.copy();
		}
	}

	public void paste() {
		if (this.textField!=null && this.textField.isFocusControl()) {
			this.textField.paste();
			this.valueChanged();
		}
	}

	public void delete() {
	}

	protected boolean isAlwaysReadOnly() {
		if(propertyEditor == null) return false;
		Object input = propertyEditor.getInput();
		if(input instanceof DefaultValueAdapter) {
			DefaultValueAdapter a = (DefaultValueAdapter)input;
			XModelObject o = a.getModelObject();
			if(o == null || o.isObjectEditable()) return false;
			while(o != null && o.getFileType() < XModelObject.FOLDER) {
				o = o.getParent();
			}
			if(o == null) return false;
			String entity = o.getModelEntity().getName();
			if(entity.indexOf("Jar") >= 0) return true;
		}
		return false;
	}

}