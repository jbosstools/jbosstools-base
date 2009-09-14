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
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.bindings.keys.ParseException;
import org.eclipse.jface.fieldassist.ComboContentAdapter;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.SimpleContentProposalProvider;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.AttributeContentProposalProviderFactory;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.border.Border;

public class ComboBoxFieldEditor extends ExtendedFieldEditor implements IFieldEditor,	IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {

	IPropertyEditor propertyEditor;

	// IValueEditor
	IValueChangeListener valueChangeListener;
	IValueProvider valueProvider;
	// IListEditor
	ILabelProvider labelProvider;
	IListContentProvider listContentProvider;
	private boolean dropDown = false;

	private String stringValue = ""; //$NON-NLS-1$
	private boolean isValid;
	public static int UNLIMITED = -1;
	private int textLimit = UNLIMITED;
	private int widthInChars = UNLIMITED;
	private Combo comboField;
	private String[] tags = new String[0];
//	private int style = SWT.NONE;
	private static final int defaultStyle = SWT.BORDER;
	boolean modifyLock = false;
	private KeyAdapter keyAdapter;
	private FocusAdapter focusAdapter;
	private ModifyListener modifyListener;
	private DisposeListener disposeListener;

	public ComboBoxFieldEditor() {
//		setStyle(SWT.DROP_DOWN | SWT.BORDER);
	}
		
	public ComboBoxFieldEditor(IWidgetSettings settings) {
		super(settings);
//		setStyle(SWT.DROP_DOWN | SWT.BORDER);
	}

	public ComboBoxFieldEditor(String name, String labelText, List<String> tags, Composite parent) {
		init(name, labelText);
		if (tags != null) {
			this.tags = tags.toArray(new String[tags.size()]);
		}
		createControl(parent);
	}
	
	protected Combo getComboField() {
		return comboField;
	}

	protected Combo getComboControl() {
		return comboField;
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)comboField.getLayoutData();
		gd.horizontalSpan = numColumns - 1;
		// We only grab excess space if we have to
		// If another field editor has more columns then
		// we assume it is setting the width.
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);

		comboField = getComboControl(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns - 1;
		if (widthInChars != UNLIMITED) {
			GC gc = new GC(comboField);
			try {
				Point extent = gc.textExtent("X");//$NON-NLS-1$
				gd.widthHint = widthInChars * extent.x;
			} finally {
				gc.dispose();
			}
		} else {
			gd.horizontalAlignment = GridData.FILL;
			gd.grabExcessHorizontalSpace = true;
		}
		comboField.setLayoutData(gd);
	}

	protected void doLoad() {
		if (comboField != null) {
			String value = getPreferenceStore().getString(getPreferenceName());
			Object listValue = mapFromTo(listContentProvider.getElements(this),getTags(),value);
			comboField.setText(listValue.toString());
			stringValue = value;
		}
	}

	protected void doLoadDefault() {
		if (comboField != null) {
			String value = getPreferenceStore().getDefaultString(getPreferenceName());
			comboField.setText(value);
		}
		valueChanged();
	}

	protected void doStore() {
		Object selectedValue = mapFromTo(getTags(),listContentProvider.getElements(this),comboField.getText()); 
		getPreferenceStore().setValue(getPreferenceName(), selectedValue.toString());
	}

	public int getNumberOfControls() {
		return 2;
	}

	protected Combo getComboControl(Composite parent) {
		if (comboField == null) {
			int style = getSettings().getStyle("Combo.Style"); //$NON-NLS-1$
			Color bg = getSettings().getColor("Combo.Background"); //$NON-NLS-1$
			if (bg==null) bg = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);// bug with gray bg
			Color fg = getSettings().getColor("Combo.Foreground"); //$NON-NLS-1$
			Font font = getSettings().getFont("Combo.Font"); //$NON-NLS-1$
			Border border = getSettings().getBorder("Combo.Border"); //$NON-NLS-1$
			if (style == SWT.DEFAULT) style = defaultStyle;
			if (isDropDown()) style = style | SWT.READ_ONLY;
						
			if (border!=null) {
				BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, border);
				comboField = new Combo(borderedControl, style);
				comboField.setBackground(bg);
			} else {
				comboField = new Combo(parent, style);
			}
			comboField.setFont(font);
			comboField.setForeground(fg);

			String[] tags = getTags();
			comboField.setItems(tags);
			if(tags != null && tags.length > 5) {
				int k = tags.length > 10 ? 10 : tags.length;
				comboField.setVisibleItemCount(k);
			}
			stringValue = valueProvider.getStringValue(true).toString();			
			comboField.setFont(parent.getFont());
			
			keyAdapter = new KeyAdapter() {
				public void keyReleased(KeyEvent e) {
					valueChanged();
				}
			};
			focusAdapter = new FocusAdapter() {
				boolean isSettingFocus = false;
				public void focusGained(FocusEvent e) {
					if(isSettingFocus) return;
					refreshValidState();
					isSettingFocus = true;
					try {
						if(comboField != null && !comboField.isDisposed()) {
							comboField.setFocus();
						}
					} finally {
						isSettingFocus = false;
					}
				}
				public void focusLost(FocusEvent e) {
					clearErrorMessage();
				}
			};
			modifyListener = new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					if(modifyLock) return;
					modifyLock = true;
					try {
						valueChanged();
						
					} finally {
						modifyLock = false;
					}
				}
			};
			disposeListener = new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					comboField = null;
				}
			};
			
			comboField.addKeyListener(keyAdapter);
			comboField.addFocusListener(focusAdapter);
			
			setStringValue(stringValue);

			comboField.addModifyListener(modifyListener);
			comboField.addDisposeListener(disposeListener);

			if(textLimit > 0){//Only set limits above 0 - see SWT spec
				comboField.setTextLimit(textLimit);
			}
			
			String[] ts = getTags();			
			Set<String> set = new TreeSet<String>();
			for (int i = 0; i < ts.length; i++) set.add(ts[i]);
			if(elements != null) for (int i = 0; i < elements.length; i++) {
				set.add(elements[i].toString());
			}
			
			
			SimpleContentProposalProvider cpp = new SimpleContentProposalProvider(set.toArray(new String[0]));
			cpp.setFiltering(true);
			KeyStroke ks = AttributeContentProposalProviderFactory.getCtrlSpaceKeyStroke();
			
			ContentProposalAdapter adapter = new ContentProposalAdapter(
				comboField,
				new ComboContentAdapter(),
					cpp,
					ks,
					null
			);
			adapter.setPropagateKeys(true);
			adapter
					.setProposalAcceptanceStyle(ContentProposalAdapter.PROPOSAL_REPLACE);
		} else {
			checkParent(comboField, parent);
		}
		return comboField;
	}

	protected void valueChanged() {
		/*added by Max Areshkau
		 *hack which was applied for fix JBIDE-1694
		 */
		if(comboField!=null&&Platform.OS_LINUX.equals(Platform.getOS())) {
			/*
			*	Fix for JBIDE-1948
			*/
			Point point = comboField.getSelection();
			comboField.setFocus();			
			comboField.setSelection(point);
		}
		setPresentsDefaultValue(false);
		boolean oldState = isValid;
		refreshValidState();

		if (isValid != oldState)
			fireStateChanged(IS_VALID, oldState, isValid);

		String oldValue = stringValue;
		int i = comboField.getSelectionIndex();		
		String newValue = (i < 0) ? comboField.getText() : elements[i].toString();
		stringValue = mapFromTo(getTags(),listContentProvider.getElements(this),newValue).toString();
		this.valueProvider.removeValueChangeListener(this);
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, stringValue);
		valueChangeListener.valueChange(event);
		revalidateValue();
		if(this.valueProvider == null) {
			//disposed already
			return;
		}
		this.valueProvider.addValueChangeListener(this);
	}
	
	/**
	 * Check that committed value is successfully assigned. 
	 * It may be rejected if destination is final object rather than wizard data. 
	 * In this case current value should be set to combo field.
	 */
	private void revalidateValue() {
		if(valueProvider == null) return;
		Object v = valueProvider.getValue();
		if(v == null || !(propertyEditor.getInput() instanceof DefaultValueAdapter)) return;
		String s = v.toString();
		String sv = stringValue;
		if(((DefaultValueAdapter)propertyEditor.getInput()).getAttribute().isTrimmable()) sv = sv.trim();
		if(!s.equals(sv)) {
			stringValue = s;
			if(!s.equals(comboField.getText())) {
				Point p = comboField.getSelection();
				boolean end = p.x == comboField.getText().length();
				comboField.setText(s);
				if(end) comboField.setSelection(new Point(comboField.getText().length(), comboField.getText().length()));
//				reset selection in the best way
			}
		}
	}
	
	Object[] elements;
	
	protected String[] getTags() {
		elements = listContentProvider.getElements(this);
		tags = new String[elements.length];
		for(int i=0;i<elements.length;++i){ 
			tags[i] = labelProvider.getText(elements[i]);
		}
		return tags;
	}
	
	protected void setStringValue(String newValue) {
		if(modifyLock) return;
		String oldValue = this.stringValue;
		stringValue = newValue;
		if (comboField != null && !isSameValue(newValue)) {
			modifyLock = true;
			comboField.setText(mapFromTo(elements,tags,newValue).toString());
			modifyLock = false;
		}
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
	}
	
	boolean isSameValue(String newValue) {
		if(comboField == null || comboField.isDisposed() || newValue == null) return false;
		String oldTextValue = comboField.getText();
		if(propertyEditor != null && propertyEditor.getInput() instanceof DefaultValueAdapter) {
			DefaultValueAdapter a = (DefaultValueAdapter)propertyEditor.getInput();
			if(a.getAttribute().isTrimmable()) {
				return oldTextValue != null && oldTextValue.trim().equals(newValue.trim());
			}
		}
		return oldTextValue != null && oldTextValue.equals(newValue);
	}
	
	static private Object mapFromTo(Object[] from, Object[] to,Object value) {
		if(from==null || from.length==0 || to==null || to.length==0) return value;
		int index = Arrays.asList(from).indexOf(value);
		return index==-1?value:to[index];
	}
	
	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
			listContentProvider = (IListContentProvider)propertyEditor.getAdapter(IListContentProvider.class);
			setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
		}
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
	}
	
	public void dispose() {
		super.dispose();
		if (comboField!=null) {
			comboField.removeKeyListener(keyAdapter);
			comboField.removeFocusListener(focusAdapter);
			comboField.removeModifyListener(modifyListener);
			comboField.removeDisposeListener(disposeListener);
			keyAdapter = null;
			focusAdapter = null;
			modifyListener = null;
			disposeListener = null;
			if (!comboField.isDisposed()) comboField.dispose();
		}
		setPropertyChangeListener(null);
		if (valueProvider!=null) valueProvider.removeValueChangeListener(this);
		propertyEditor = null;
		valueChangeListener = null;
		valueProvider = null;
		labelProvider = null;
		listContentProvider = null;
	}
	
	// IPropertyChangeListener
	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			PropertyChangeEvent e = new PropertyChangeEvent(this, IPropertyEditor.VALUE, mapFromTo(tags,elements,event.getOldValue()), mapFromTo(tags,elements,event.getNewValue()));
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), getComboControl(parent)};
	}

	// PropertyChangeListener
	public void propertyChange(PropertyChangeEvent evt) {
		super.propertyChange(evt);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			valueProvider.removeValueChangeListener(this);
			this.setStringValue((v == null) ? "" : v.toString()); //$NON-NLS-1$
			valueProvider.addValueChangeListener(this);
		}
		if (IPropertyEditor.LIST_CONTENT.equals(evt.getPropertyName())) {
			String v = comboField.getText();
			valueProvider.removeValueChangeListener(this);
			String[] tags = getTags();
			comboField.setItems(tags);
			comboField.setText(v);
			int i = comboField.getSelectionIndex();
			valueProvider.addValueChangeListener(this);
			if(i < 0 && tags != null && tags.length > 0) {
				comboField.setText(tags[0]);
			} else if(i < 0) {
				comboField.setText(""); //$NON-NLS-1$
			}
		}
	}
	
	public void setEnabled(boolean enabled){
		super.setEnabled(enabled); // label
		if (this.getComboControl()!=null) {
			this.getComboControl().setEnabled(enabled);
			Color bg;
			if (enabled) {
				bg = getSettings().getColor("Combo.Background"); //$NON-NLS-1$
				if (bg==null) bg = Display.getDefault().getSystemColor(SWT.COLOR_WHITE);
			} else {
				bg = getSettings().getColor("Combo.Background.Disabled"); //$NON-NLS-1$
				if (bg==null) bg = Display.getDefault().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
			} 
///			this.getComboControl().setBackground(bg);
			updateErrorState();
		}
	}

	public void cut() {
		if(comboField != null && !comboField.isDisposed() && comboField.isFocusControl()) {
			Text text = getInnerText();
			if(text != null) text.cut();
			valueChanged();
		}
	}

	public void copy() {
		if(comboField != null && !comboField.isDisposed() && comboField.isFocusControl()) {
			Text text = getInnerText();
			if(text != null) text.copy();
		}
	}

	public void paste() {
		if(comboField != null && !comboField.isDisposed() && comboField.isFocusControl()) {
			Text text = getInnerText();
			if(text != null) text.paste();
			valueChanged();
		}
	}

	public void delete() {
	}
	
	private Text getInnerText() {
		try {
			Field f = comboField.getClass().getDeclaredField("text"); //$NON-NLS-1$
			f.setAccessible(true);
			Text text = (Text)f.get(comboField);
			return (text != null && !text.isDisposed()) ? text : null;
		} catch (NoSuchFieldException e) {
			return null;
		} catch (IllegalAccessException e1) {
			return null;
		}
	}
	/**
	 * @return
	 */
	public boolean isDropDown() {
		return dropDown;
	}

	/**
	 * @param b
	 */
	public void setDropDown(boolean b) {
		dropDown = b;
	}

	public void setFocus() {
		if (comboField != null) {
			comboField.getParent().setFocus();
			comboField.setSelection(new Point(0, comboField.getText().length()));
			comboField.setFocus();
		}
	}
}
