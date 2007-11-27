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
import java.util.Arrays;
import java.util.List;
import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.*;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.util.*;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import org.jboss.tools.common.model.ui.widgets.BorderedControl;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.border.Border;

public class RadioArrayFieldEditor extends ExtendedFieldEditor implements IFieldEditor,	IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	IPropertyEditor propertyEditor;
	IValueChangeListener valueChangeListener;
	IValueProvider valueProvider;
	ILabelProvider labelProvider;
	IListContentProvider listContentProvider;

	private String stringValue = "";
	private boolean isValid;
	private Composite panel;
	private Button[] radioButtons;
	private String[] tags = new String[0];
	private int style = SWT.NONE;
	boolean modifyLock = false;

	public RadioArrayFieldEditor() {
//		setStyle(SWT.DROP_DOWN | SWT.BORDER);
	}
		
	public RadioArrayFieldEditor(IWidgetSettings settings) {
		super(settings);
//		setStyle(SWT.DROP_DOWN | SWT.BORDER);
	}

	public RadioArrayFieldEditor(String name, String labelText, List<String> tags, Composite parent) {
		init(name, labelText);
		if (tags != null) {
			this.tags = tags.toArray(new String[tags.size()]);
		}
		createControl(parent);
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)panel.getLayoutData();
		gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		panel = getPanelControl(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns - 1;
		gd.horizontalAlignment = GridData.FILL;
		gd.grabExcessHorizontalSpace = true;
		panel.setLayoutData(gd);
	}

	public int getNumberOfControls() {
		return 2;
	}

	public Control[] getControls(Composite parent) {
		return new Control[] {new Label(parent,SWT.NONE), getPanelControl(parent)};
	}

	public Composite getPanelControl(Composite parent) {
		if (panel == null) {
/*
			int style = getSettings().getInt("Combo.Style");
			Color bg = getSettings().getColor("Combo.Background");
			if (bg==null) bg = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);// bug with gray bg
			Color fg = getSettings().getColor("Combo.Foreground");
			Font font = getSettings().getFont("Combo.Font");
*/			
			Border border = getSettings().getBorder("Combo.Border");
			boolean makeGroup = makeGroup();
			if (border!=null) {
				BorderedControl borderedControl = new BorderedControl(parent, SWT.NONE, border);
				if(!makeGroup) {
					panel = new Composite(borderedControl, SWT.FLAT);
				} else {
					Group g = new Group(borderedControl, SWT.SHADOW_ETCHED_IN);
					g.setText(propertyEditor.getLabelText());
					panel = g;
				}
			} else {
				if(!makeGroup) {
					panel = new Composite(parent, getStyle());
				} else {
					Group g = new Group(parent, SWT.SHADOW_ETCHED_IN);
					g.setText(propertyEditor.getLabelText());
					panel = g;
				}
			}
			/// change!
			int k = getTags().length;
			if(k != 3 || makeGroup) k = 1;
			GridLayout layout = new GridLayout(k, false);
			panel.setLayout(layout);
//			panel.setFont(font);
//			panel.setBackground(bg);
//			panel.setForeground(fg);

			modifyLock = true;
			setItems(getTags());
			stringValue = valueProvider.getStringValue(true);
			setSelectedValue(stringValue);
			modifyLock = false;

			panel.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					panel = null;
					radioButtons = new Button[0];
				}
			});
		} else {
			///checkParent(panel, parent);
		}
		return panel;
	}
	
	boolean makeGroup() {
		Object input = propertyEditor.getInput();
		if(input instanceof DefaultValueAdapter) {
			DefaultValueAdapter adapter = (DefaultValueAdapter)input;
			return "true".equals(adapter.getAttribute().getProperty("border"));
		}
		return false;
	}

	protected void valueChanged() {
		setPresentsDefaultValue(false);
		boolean oldState = isValid;
		refreshValidState();
		if (isValid != oldState)
			fireStateChanged(IS_VALID, oldState, isValid);
		String oldValue = stringValue;
		String newValue = getSelectedValue();
		this.valueProvider.removeValueChangeListener(this);
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
		stringValue = newValue;
		this.valueProvider.addValueChangeListener(this);
	}
	Object[] elements;
	
	private String[] getTags() {
		elements = listContentProvider.getElements(this);
		tags = new String[elements.length];
		for(int i=0;i<elements.length;++i){ 
			tags[i] = labelProvider.getText(elements[i]);
		}
		return tags;
	}

	public int getStyle() {
		return style;
	}
	public void setStyle(int i) {
		style = i;
	}
	
	protected void setStringValue(String newValue) {
		String oldValue = this.stringValue;
		stringValue = newValue;
		modifyLock = true;
		setSelectedValue(newValue);
		modifyLock = false;
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
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
		}
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
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

	// PropertyChangeListener
	public void propertyChange(PropertyChangeEvent evt) {
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			valueProvider.removeValueChangeListener(this);
			this.setStringValue((v == null) ? "" : v.toString());
			valueProvider.addValueChangeListener(this);
		}
		if (IPropertyEditor.LIST_CONTENT.equals(evt.getPropertyName())) {
			String v = getSelectedValue();
			valueProvider.removeValueChangeListener(this);
			String[] tags = getTags();
			setItems(tags);
			setSelectedValue(v);
		}
	}
	
	private void setItems(String[] tags) {
		this.tags = tags;
		if(radioButtons != null) { 
			for (int i = 0; i < radioButtons.length; i++) {
				radioButtons[i].dispose();
			}
		}
		if(panel == null) return;
		radioButtons = new Button[tags.length];
		for (int i = 0; i < tags.length; i++) {
			radioButtons[i] = createRadio(tags[i]);
		}
	}
	
	private Button createRadio(String tag) {
		Button b = new Button(panel, SWT.RADIO);
		GridData d = new GridData(GridData.FILL_HORIZONTAL);
		b.setLayoutData(d);
		b.setText(tag);
		b.addSelectionListener(new SL(b));
		return b;
	}
	
	class SL implements SelectionListener {
		Button b;
		SL(Button b) {
			this.b = b;
		}
		public void widgetSelected(SelectionEvent e) {
			if(modifyLock || radioButtons == null) return;
			modifyLock = true;
			for (int i = 0; i < radioButtons.length; i++) {
				if(radioButtons[i] != b) radioButtons[i].setSelection(false);
			}
			setSelectedValue(b.getText());
			valueChanged();
			modifyLock = false;
		}
		public void widgetDefaultSelected(SelectionEvent e) {}		
	}
	
	private String getSelectedValue() {
		if(radioButtons == null) return "";
		for (int i = 0; i < radioButtons.length; i++) {
			if(radioButtons[i].getSelection()) return tags[i];
		}
		return "";
	}
	
	private void setSelectedValue(String v) {
		if(radioButtons == null) return;
		for (int i = 0; i < radioButtons.length; i++) {
			radioButtons[i].setSelection(v.equals(tags[i]));
		}
	}
	
	public void setEnabled(boolean enabled, Composite parent) {
		if(radioButtons == null) return;
		for (int i = 0; i < radioButtons.length; i++) {
			radioButtons[i].setEnabled(enabled);
		}
	}

	protected void doLoad() {}

	protected void doLoadDefault() {}

	protected void doStore() {}

	public void cut() {}

	public void copy() {}

	public void paste() {}

	public void delete() {}

}
