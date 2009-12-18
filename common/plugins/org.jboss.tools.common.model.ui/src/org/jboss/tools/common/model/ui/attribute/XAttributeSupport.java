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
package org.jboss.tools.common.model.ui.attribute;
 

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.jboss.tools.common.model.ui.attribute.adapter.AdapterFactory;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.ExtendedFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.IFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.IPropertyEditor;
import org.jboss.tools.common.model.ui.attribute.editor.NoteEditor;
import org.jboss.tools.common.model.ui.attribute.editor.NoteFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditor;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditorFactory;
import org.jboss.tools.common.model.ui.attribute.editor.StringButtonFieldEditorEx;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.actions.ActionFactory;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author eskimo
 */
public class XAttributeSupport {
	boolean isDebugging = ModelUIPlugin.getDefault().isDebugging();

	private ArrayList<IModelPropertyEditorAdapter> adapters = new ArrayList<IModelPropertyEditorAdapter>();
	private ArrayList<PropertyEditor> editors = new ArrayList<PropertyEditor>();
	private Map<String,FieldEditor> fieldEditors = new HashMap<String,FieldEditor>();
	protected Set<String> alwaysGreedy = new HashSet<String>();
	
	private XModelObject xmo;
	private XEntityData data;
	
	private Layout layout;
	
	private IWidgetSettings settings = DefaultSettings.getDefault();

	public XAttributeSupport() {
	}

	public XAttributeSupport(IWidgetSettings settings) {
		this.settings = settings;
	}

	public XAttributeSupport(XModelObject xmo) {
		if(xmo==null) throw new IllegalArgumentException("XModelObject cannot be null."); //$NON-NLS-1$
		init(xmo);
	}

	public XAttributeSupport(XModelObject xmo, XEntityData data) {
		this(xmo, data, false);
	}
	
	public XAttributeSupport(XModelObject xmo, XEntityData data, boolean useObject) {
		if(data==null) throw new IllegalArgumentException("XEntityData cannot be null."); //$NON-NLS-1$
		init(xmo, data, useObject);
	}
	
	public XEntityData getEntityData() {
		return data;
	}

	public void init(XModelObject xmo, XEntityData data) {
		init(xmo, data, false);
	}

	public void init(XModelObject xmo) {    	
		this.xmo = xmo;
		XAttribute[] attribute = xmo.getModelEntity().getAttributes();
		    
		adapters.clear();
		editors.clear();
		
		for (int i = 0; i < attribute.length; i++) {
			if(!attribute[i].isVisible()) continue;
			IModelPropertyEditorAdapter adapter = createAdapter(xmo, attribute[i]);
			PropertyEditor editor = createEditor(adapter, xmo, attribute[i]);
			adapters.add(adapter);
			editors.add(editor);
			if("always".equals(attribute[i].getProperty("greedy"))) { //$NON-NLS-1$ //$NON-NLS-2$
				alwaysGreedy.add(attribute[i].getName());
			}
		}
	}

	public void init(XModelObject xmo, XEntityData data, boolean useObject) {
		this.data = data;
		this.xmo = xmo;
		XAttributeData ads[] = data.getAttributeData();
		
		adapters.clear();
		editors.clear();
		
		for (int i = 0; i < ads.length; i++) {
			//if(!ads[i].getAttribute().isVisible()) continue;
			XAttribute attr = ads[i].getAttribute();
			IModelPropertyEditorAdapter adapter = (useObject) ? createAdapter(xmo, attr) : createAdapter(xmo, ads[i]);
			PropertyEditor editor = (useObject) ? createEditor(adapter, xmo, attr) : createEditor(adapter, xmo, ads[i]);
			String labelText = WizardKeys.getAttributeDisplayName(ads[i]);
			if (labelText!=null) {
				labelText = labelText + ((ads[i].getMandatoryFlag()) ? "*" : ""); //$NON-NLS-1$ //$NON-NLS-2$
				editor.setLabelText(labelText);
			}
			adapters.add(adapter);
			
			editors.add(editor);

			if("always".equals(attr.getProperty("greedy"))) { //$NON-NLS-1$ //$NON-NLS-2$
				alwaysGreedy.add(attr.getName());
			}
		}
		
	}
	
	public void dispose() {
		if (editors!=null) {
			Iterator i = editors.iterator();
			Object object;
			while (i.hasNext()) {
				object = i.next();
				if (object instanceof FieldEditor) ((FieldEditor)object).dispose();
			}
			editors.clear();
			editors = null;
		}
		if (adapters!=null) {
			Iterator i = adapters.iterator();
			Object object;
			while (i.hasNext()) {
				object = i.next();
				if (object instanceof DefaultValueAdapter) ((DefaultValueAdapter)object).dispose();
			}
			adapters.clear();
			adapters = null;
		}
		if (fieldEditors!=null) {
			fieldEditors.clear();
			fieldEditors = null;
		}
		if (alwaysGreedy!=null) {
			alwaysGreedy.clear();
			alwaysGreedy = null;
		}
		xmo = null;
		settings = null;
	}
	
	private IModelPropertyEditorAdapter createAdapter(XModelObject o, XAttribute a) {
		return AdapterFactory.getAdapter(a, o, o.getModel());
	}
	private IModelPropertyEditorAdapter createAdapter(XModelObject o, XAttributeData a) {
		return AdapterFactory.getAdapter(a.getAttribute(), o, a, o.getModel()); 
	}
	private PropertyEditor createEditor(Object adapter, XModelObject o, XAttribute a) {
		return PropertyEditorFactory.createPropertyEditor(adapter, a, o, settings);
	}
	private PropertyEditor createEditor(Object adapter, XModelObject o, XAttributeData a) {
		return PropertyEditorFactory.createPropertyEditor(adapter, a.getAttribute(), a, settings); 
	}

		
	public void setAutoStore(boolean set) {
		for (Iterator iter = adapters.iterator(); iter.hasNext();) {
			IModelPropertyEditorAdapter element = (IModelPropertyEditorAdapter) iter.next();
			element.setAutoStore(set);
		}
	}
	
//	private static final int COLUMN_1_WIDTH = 100;
	
	public Control[] fillComposite(Composite composite, PropertyEditor editor, StringButtonFieldEditorEx sb) {
		composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
		
		ExtendedFieldEditor fieldEditor = null;
		boolean greedy = editor.isGreedyEditor();
		if(!greedy) { 
			fieldEditor = editor.getFieldEditor(composite);
		} else {
			if(sb == null) sb = new StringButtonFieldEditorEx(settings);       
			sb.setLabelText(editor.getLabelText());
			sb.setPropertyEditor(editor);
			String changeButtonName = editor.getChangeButtonName();
			if(changeButtonName != null) {
				sb.setChangeButtonText(changeButtonName);
			}
			fieldEditor = sb;
			greedy = false;
		}
		registerFieldEditor(editor.getAttributeName(), fieldEditor);
		if (!enabled) {
			fieldEditor.setEnabled(enabled);
		}
		Control[] controls = fillComposite(composite, fieldEditor, greedy);
		if(fieldEditor instanceof StringButtonFieldEditorEx) {
			((StringButtonFieldEditorEx)fieldEditor).setStringValue("" + editor.getValue()); //$NON-NLS-1$
		}
		return controls;
	}
	
	public Control[] fillComposite(Composite composite, FieldEditor fieldEditor, boolean greedy) {

			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
		
		if (fieldEditor instanceof IFieldEditor) {
			Control control;
			GridData gd;
			Control[] controls = ((IFieldEditor)fieldEditor).getControls(composite);
			if (controls.length==2) {
				// layout 2 element
				control = controls[0];
				gd = new GridData();
				control.setLayoutData(gd);

				control = controls[1];
				if(greedy) {
					gd.verticalAlignment = GridData.BEGINNING;
					if(editors.size() == 1) gd.horizontalSpan = 2;
					gd = new GridData(GridData.FILL_BOTH);
					if(editors.size() == 1) gd.horizontalSpan = 2;
				} else {
					gd = new GridData(GridData.FILL_HORIZONTAL);
				}
				if(greedy) {
					int h = control.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
					if(h > 300) gd.heightHint = 300;
					if (fieldEditor instanceof NoteFieldEditor) {
						gd.heightHint = 100;
					}
				}
				control.setLayoutData(gd);
			} else if (controls.length==1) {
				// layout 1st element
				control = controls[0];
				gd = new GridData(GridData.FILL_HORIZONTAL);
				gd.horizontalSpan = 2;
				
				control.setLayoutData(gd);
			}
			return controls;
		} else {
			fieldEditor.fillIntoGrid(composite, 2);
			return null;
		}
	}
	
	public void fillComposite(Composite composite) {
		
			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
		
		int greedyCount = 0;
		sortEditors();
		for (int i = 0; i < editors.size(); i++) {
			PropertyEditor editor = (PropertyEditor)editors.get(i);
			if(editor.isGreedyEditor()) ++greedyCount;
		}   
		for (int i = 0; i < editors.size(); i++) {
			PropertyEditor editor = (PropertyEditor)editors.get(i);
			//editor.setLabelText(support.getAttributeMessage(id, ads[i].getAttribute().getName()));
			ExtendedFieldEditor fieldEditor = null;
			boolean greedy = editor.isGreedyEditor();
			if(!greedy || keepGreedy(editor.getAttributeName(), i, greedyCount) || editor instanceof NoteEditor) { 
				fieldEditor = editor.getFieldEditor(composite);
			} else {
				StringButtonFieldEditorEx sb;

				sb = new StringButtonFieldEditorEx(settings);       

				sb.setLabelText(editor.getLabelText());
				sb.setPropertyEditor(editor);
				String changeButtonName = editor.getChangeButtonName();
				if(changeButtonName != null) {
					sb.setChangeButtonText(changeButtonName);
				}
				fieldEditor = sb;
				greedy = false;
			}
			registerFieldEditor(editor.getAttributeName(), fieldEditor);
			if (!enabled) {
				fieldEditor.setEnabled(enabled);
			}
			fillComposite(composite, fieldEditor, greedy);
			if(fieldEditor instanceof StringButtonFieldEditorEx) {
				((StringButtonFieldEditorEx)fieldEditor).setStringValue("" + editor.getValue()); //$NON-NLS-1$
			}
		}
	}
	
	private void sortEditors(){
		PropertyEditor editor = null;
		for (int i = 0; i < editors.size(); i++) {
			editor = editors.get(i);
			if ("description".equalsIgnoreCase(editor.getAttributeName())) {
				editors.remove(editor);
				editors.add(editor);
				return;
			}
		}
	}

	public Composite createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);

//		composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
//		composite.setBackground(parent.getBackground());
		
		composite.setLayout(getLayout());
		fillComposite(composite);
//		int greedyCount = 0;
//		sortEditors();
//		for (int i = 0; i < editors.size(); i++) {
//			PropertyEditor editor = (PropertyEditor)editors.get(i);
//			if(editor.isGreedyEditor()) ++greedyCount;
//		}   
//
//		for (int i = 0; i < editors.size(); i++) {
//			PropertyEditor editor = (PropertyEditor)editors.get(i);
//			//editor.setLabelText(support.getAttributeMessage(id, ads[i].getAttribute().getName()));
//			ExtendedFieldEditor fieldEditor = null;
//			boolean greedy = editor.isGreedyEditor();
//			if(!greedy || keepGreedy(editor.getAttributeName(), i, greedyCount) || editor instanceof NoteEditor) { 
//				fieldEditor = editor.getFieldEditor(composite);
//			} else {
//				StringButtonFieldEditorEx sb = null;       
//
//				sb = new StringButtonFieldEditorEx(settings);
//
//				sb.setLabelText(editor.getLabelText());
//				String changeButtonName = editor.getChangeButtonName();
//				if(changeButtonName != null) {
//					sb.setChangeButtonText(changeButtonName);
//				}
//				sb.setPropertyEditor(editor);
//				fieldEditor = sb;
//				greedy = false;
//			}
//			registerFieldEditor(editor.getAttributeName(), fieldEditor);
//			if (!enabled) {
//				fieldEditor.setEnabled(enabled);
//			}
//			fillComposite(composite, fieldEditor, greedy);
//			if(fieldEditor instanceof StringButtonFieldEditorEx) {
//				((StringButtonFieldEditorEx)fieldEditor).setStringValue("" + editor.getValue()); //$NON-NLS-1$
//			}
//		}
		return composite;
	}
	
	protected boolean keepGreedy(String name, int index, int greedyCount) {
		return greedyCount < 1 || (greedyCount == 1 && editors.size() < 2)
		       || alwaysGreedy.contains(name);
	}

	public void store() {
		for (int i = 0; i < editors.size(); i++)
		  ((IModelPropertyEditorAdapter)adapters.get(i)).store();
	}

	public void load() {
		if(xmo == null) throw new IllegalArgumentException("" + //$NON-NLS-1$
			"Init support with XModelObject before saving");		 //$NON-NLS-1$
		for (int i = 0; i < editors.size(); i++)
		((IModelPropertyEditorAdapter)adapters.get(i)).load();
	}	
	
	public void save() {
		if(xmo == null) throw new IllegalArgumentException("" + //$NON-NLS-1$			"Init support with XModelObject before saving"); //$NON-NLS-1$
		if(xmo.getModel()==null) throw new IllegalArgumentException("Cannot store deleted or removed XModel object"); //$NON-NLS-1$
		xmo.getModel().saveOptions();	
	}
	
	public List getAdapterList() {
		return Collections.unmodifiableList(adapters);
	}
	
	public List<FieldEditor> getVisibleFieldEditors() {
		ArrayList<FieldEditor> visibleEditors = new ArrayList<FieldEditor>();
		Set keySet = fieldEditors.keySet();
		Iterator i = keySet.iterator();
		while (i.hasNext()) visibleEditors.add(fieldEditors.get(i.next()));
		return visibleEditors; 
	}
	
	public List getEditorList() {
		return Collections.unmodifiableList(editors);
	}
	
	public String getTitle() {
		return xmo.getPresentationString();
	}
	
	public IPropertyEditor getPropertyEditorByName(String attributeName) {
		IPropertyEditor result = null;
		if (attributeName != null && !"".equals(attributeName)) { //$NON-NLS-1$
			Iterator iterator = editors.iterator();
			while (iterator.hasNext() && result == null) {
				IPropertyEditor editor = (IPropertyEditor)iterator.next();
				if (attributeName.equals(editor.getAttributeName()))
					result = editor;
			}
		}
		return result;
	}
	
	public FieldEditor getFieldEditorByName(String attributeName) {
		return (FieldEditor)fieldEditors.get(attributeName);
	}

	public void registerFieldEditor(String attributeName, FieldEditor f) {
		if(f != null) fieldEditors.put(attributeName, f);
	}
	
	public IModelPropertyEditorAdapter getPropertyEditorAdapterByName(String attributeName)	{
		IModelPropertyEditorAdapter result = null;		
		if (attributeName != null && !"".equals(attributeName)) { //$NON-NLS-1$
			Iterator iterator = adapters.iterator();
			while (iterator.hasNext() && result == null) {
				IModelPropertyEditorAdapter adapter = (IModelPropertyEditorAdapter)iterator.next();
				if (attributeName.equals(adapter.getAttribute().getName()))
					result = adapter;
			}
		}
		return result;		
	}

	public Layout getLayout() {
		if (layout == null) {
			layout = getDefaultLayout();
		}
		return layout;
	}

	public void setLayout(Layout layout) {
		this.layout = layout;
	}
	
	protected Layout getDefaultLayout() {
		GridLayout gridLayout = new GridLayout(2, false);
		return gridLayout;
	}
	
	public void addPropertyChangeListener(java.beans.PropertyChangeListener listener) {
		for (int i = 0; i < adapters.size(); i++) {
			IModelPropertyEditorAdapter a = (IModelPropertyEditorAdapter)adapters.get(i);
			a.addValueChangeListener(listener);
		}
	}

	public void removePropertyChangeListener(java.beans.PropertyChangeListener listener) {
		for (int i = 0; i < adapters.size(); i++) {
			IModelPropertyEditorAdapter a = (IModelPropertyEditorAdapter)adapters.get(i);
			a.removeValueChangeListener(listener);
		}
	}
	
	public Properties getValues() {
		Properties p = new Properties();
		if(data == null) return p;
		XAttributeData ads[] = data.getAttributeData();
		for (int i = 0; i < adapters.size(); i++) {
			IModelPropertyEditorAdapter a = (IModelPropertyEditorAdapter)adapters.get(i);
			Object v = a.getValue();
			if(v == null) continue;
			XAttribute att = ads[i].getAttribute();
			String value = v.toString();
			if(att.isTrimmable()) value = value.trim();
			p.setProperty(att.getName(), value);
		}
		return p;		
	}

	/**
	 * @return
	 */
	public IWidgetSettings getSettings() {
		return settings;
	}

	/**
	 * @param settings
	 */
	public void setSettings(IWidgetSettings settings) {
		this.settings = settings;
	}
	
	/*
	 * enabled
	 */
	private boolean enabled = Boolean.TRUE.booleanValue();

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean b) {
		enabled = b;
		doSetEnabled();
	}
	
	protected void doSetEnabled() {
		if (this.fieldEditors!=null) {
			Collection fieldEditros = this.fieldEditors.values();
			Iterator i = fieldEditros.iterator();
			while (i.hasNext()) {
				((ExtendedFieldEditor)i.next()).setEnabled(enabled);
			}
		}
	}
	
	public void updateEnablementByModelObject() {
		Iterator it = fieldEditors.keySet().iterator();
		while(it.hasNext()) {
			String n = it.next().toString();
			ExtendedFieldEditor f = (ExtendedFieldEditor)fieldEditors.get(n);
			f.setEnabled(xmo.isAttributeEditable(n));
		}
	}

	public boolean doGlobalAction(String actionId) {
		if (getVisibleFieldEditors()==null) return false;
		Iterator i = getVisibleFieldEditors().iterator();
		ExtendedFieldEditor fieldEditor;
		while (i.hasNext()) {
			fieldEditor = (ExtendedFieldEditor)i.next();
			if (ActionFactory.CUT.getId().equals(actionId)) {
				fieldEditor.cut();
			} else if (ActionFactory.COPY.getId().equals(actionId)) {
				fieldEditor.copy();
			} else if (ActionFactory.PASTE.getId().equals(actionId)) {
				fieldEditor.paste();
			} else if (ActionFactory.DELETE.getId().equals(actionId)) {
				fieldEditor.delete();
			}
		}
		return false;
	}
	
}
