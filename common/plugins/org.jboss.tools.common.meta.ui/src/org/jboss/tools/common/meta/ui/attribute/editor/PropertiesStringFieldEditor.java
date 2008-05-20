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
package org.jboss.tools.common.meta.ui.attribute.editor;

import java.beans.PropertyChangeListener;
import java.util.StringTokenizer;
import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.editor.*;
import org.jboss.tools.common.model.ui.objecteditor.*;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.util.AbstractTableHelper;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class PropertiesStringFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	protected IPropertyEditor propertyEditor;
	
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;

	public static final int VALIDATE_ON_KEY_STROKE = 0;
	public static final int VALIDATE_ON_FOCUS_LOST = 1;
	public static int UNLIMITED = -1;
	private boolean isValid;
	protected String stringValue;
	private String oldValue;
	private XModelObject object;
	private XChildrenEditor table;
	private boolean emptyStringAllowed = true;
	private int validateStrategy = VALIDATE_ON_KEY_STROKE;
	
	public PropertiesStringFieldEditor() {
		this(null);
	}

	public PropertiesStringFieldEditor(IWidgetSettings settings) {
		super(settings);
		object = PreferenceModelUtilities.getPreferenceModel().createModelObject("FilePROPERTIES", null);
		table = new XChildrenEditorImpl();
		table.setObject(object);
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)table.getControl().getLayoutData();
		gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
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

		Control c = createObjectControl(parent);
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		gd.horizontalAlignment = GridData.FILL;
		gd.grabExcessHorizontalSpace = true;
		c.setLayoutData(gd);
	}

	protected void doLoad() {}

	public String getStringValue() {
		return stringValue;
	}
	
	public void setStringValue(String value) {
		if(stringValue != null && stringValue.equals(value)) return;
		stringValue = value;
		XModelObject[] cs = object.getChildren();
		for (int i = 0; i < cs.length; i++) {
			cs[i].removeFromParent();
		}
		StringTokenizer st = new StringTokenizer(value, ",;");
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			int h = t.indexOf('=');
			String n = (h < 0) ? t : t.substring(0, h);
			String v = t.substring(h + 1);
			XModelObject p = object.getModel().createModelObject("Property", null);
			p.setAttributeValue("name", n);
			p.setAttributeValue("value", v);
			object.addChild(p);
		}
		table.update();
	}

	protected void doLoadDefault() {}

	protected void doStore() {}

	public int getNumberOfControls() {
		return 2;
	}

	public void cut() {}

	public void copy() {}

	public void paste() {}

	public void delete() {}

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createObjectControl(parent)};
	}
	
	protected Control createObjectControl(Composite parent) {
		if(table.getControl() != null && !table.getControl().isDisposed()) return table.getControl();
		return table.createControl(parent);		
	}

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
		init();
		valueProvider.addValueChangeListener(this);
	}

	protected void init() {
		setStringValue(valueProvider.getStringValue(true));
		setPropertyChangeListener(this);
	}

	public void propertyChange(PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			java.beans.PropertyChangeEvent e = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, event.getOldValue(), event.getNewValue());
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}

	public void propertyChange(java.beans.PropertyChangeEvent evt) {
	}
	
	class XChildrenEditorImpl extends XChildrenEditor {
		protected AbstractTableHelper createHelper() {
			return new Helper();
		}
		protected String getAddActionPath() {
			return "CreateActions.CreateProperty";
		}
		public void action(String command) {
			super.action(command);
			String oldValue = stringValue;
			recomputeStringValue();
			if(oldValue == null || !oldValue.equals(stringValue)) {
				fireValueChanged(VALUE, oldValue, stringValue);
			}
		}
	}
	
	private void recomputeStringValue() {
		StringBuffer sb = new StringBuffer();
		XModelObject[] os = object.getChildren();
		for (int i = 0; i < os.length; i++) {
			String n = os[i].getAttributeValue("name");
			String v = os[i].getAttributeValue("value");
			if(n.length() == 0 && v.length() == 0) continue;
			if(sb.length() > 0) sb.append(';');
			sb.append(n).append('=').append(v);
		}
		stringValue = sb.toString();
	}

	class Helper extends AbstractTableHelper {
		String[] HEADER = new String[]{"name", "value"};
		
		public String[] getHeader() {
			return HEADER;
		}
	    
		public int size() {
			return (object == null) ? 0 : object.getChildren().length;
		}

		public XModelObject getModelObject(int r) {
			if(object == null) return null;
			XModelObject[] cs = object.getChildren();
			return (r < 0 || r >= cs.length) ? null : cs[r];
		}
	}

}
