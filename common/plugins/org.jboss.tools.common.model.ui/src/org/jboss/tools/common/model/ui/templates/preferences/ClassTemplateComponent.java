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
package org.jboss.tools.common.model.ui.templates.preferences;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;

import org.jboss.tools.common.model.ui.attribute.adapter.ColumnDescription;
import org.jboss.tools.common.model.ui.attribute.adapter.CompositeActionProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultTableStructuredAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.IFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.JavaChoicerEditor;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditor;
import org.jboss.tools.common.model.ui.attribute.editor.StringButtonFieldEditorEx;
import org.jboss.tools.common.model.ui.attribute.editor.TableStructuredEditor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.ui.templates.model.MetaClassTemplate;
import org.jboss.tools.common.model.ui.templates.model.MetaValue;

public class ClassTemplateComponent {
	XModel model;
    // xpath
	Label xPathLabelValue;
	String initValue = "";
    // base class
    JavaChoicerEditor baseClassEditor;
    BaseClassAdapter baseClassAdapter;
    BaseClassAdapterListener baseClassAdapterListener;
    // interfaces
    TableStructuredEditor interfacesEditor;
    DefaultTableStructuredAdapter interfacesAdapter;
    CompositeActionProvider interfaceActions;

	MetaClassTemplate selectedTemplate = null;

	Composite composite;

    protected Control createContents(Composite composite) {
    	this.composite = composite;
        GridData gd;
        Control[] control;

        gd = new GridData(GridData.FILL_HORIZONTAL);
        Label xPathLabel = new Label(composite, SWT.NONE);
        xPathLabel.setLayoutData(gd);
        xPathLabel.setText("XPath");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        xPathLabelValue = new Label(composite, SWT.NONE);
        xPathLabelValue.setLayoutData(gd);
        xPathLabelValue.setText(initValue);

        // base class
        baseClassEditor.setLabelText("Base Class");

        StringButtonFieldEditorEx sb;
		sb = new StringButtonFieldEditorEx();
		sb.setLabelText(baseClassEditor.getLabelText());
		sb.setPropertyEditor(baseClassEditor);
		String changeButtonName = baseClassEditor.getChangeButtonName();
		if(changeButtonName != null) {
			sb.setChangeButtonText(changeButtonName);
		}

        control = sb.getControls(composite);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        control[1].setLayoutData(gd);

        // interfaces
        interfacesEditor.setLabelText("Interfaces");
        control = getControls(composite, interfacesEditor);
        control[0].dispose(); // dispose empty label
        gd = new GridData(GridData.FILL_BOTH);
        gd.horizontalSpan = 2;
        control[1].setLayoutData(gd);

        return composite;
    }

    public void setModel(XModel model) {
    	this.model = model;

	    // base class
	    baseClassEditor = new JavaChoicerEditor();
	    baseClassAdapter = new BaseClassAdapter();
	    baseClassAdapter.setModel(model);
	    baseClassAdapterListener = new BaseClassAdapterListener();
	    baseClassAdapter.addValueChangeListener(baseClassAdapterListener);
	    baseClassEditor.setInput(baseClassAdapter);
	    // interfaces
	    interfacesEditor = new TableStructuredEditor();
	    interfacesAdapter = new DefaultTableStructuredAdapter();
	    interfaceActions = new ActionProvider();
	    interfacesAdapter.setActionProvider(interfaceActions);
	    interfacesAdapter.addColumnDescription(new ColumnDescription("Interfaces", null, 100, SWT.LEFT, true, null));
	    interfacesAdapter.setTableLabelProvider(new TableLabelProvider());
	    interfacesEditor.setInput(interfacesAdapter);
    }

	class BaseClassAdapter extends DefaultValueAdapter {
		MetaValue value;
		public void load() {
			if(value == null) {
				setValue("");
			} else {
				String v = value.getValue();
				if(v == null) v = "java.lang.Object";
				setValue(v);
			}
		}
		public void store() {
			if(isStoreLocked()) return;
			if(value != null) value.setValue(getStringValue(true));
		}
	}

	class BaseClassAdapterListener implements PropertyChangeListener {
		MetaValue value;
		public void propertyChange(PropertyChangeEvent evt) {
			if(value != null) baseClassAdapter.store();
		}
    }

	private void addInterface() {
		if(selectedTemplate == null) return;
		String s = AddInterfaceSupport.runAdd(model);
		if(s == null) return;
		String[] vs = selectedTemplate.getInterfaces().getValues();
		if(vs == null) vs = new String[0];
		String[] vsn = new String[vs.length + 1];
		System.arraycopy(vs, 0, vsn, 0, vs.length);
		vsn[vs.length] = s;
		selectedTemplate.getInterfaces().setValues(vsn);
		interfacesAdapter.setValue(selectedTemplate.getInterfaces().getValues());
	}

	private void removeInterface() {
		if(selectedTemplate == null) return;
		ISelection s = interfacesAdapter.getSelection();
		if(s.isEmpty()) return;
		ArrayList<String> l = new ArrayList<String>();
		String[] vs = selectedTemplate.getInterfaces().getValues();
		if(vs == null) return;
		if(vs != null) for (int i = 0; i < vs.length; i++) l.add(vs[i]);
		Iterator i = ((StructuredSelection)s).iterator();
		while(i.hasNext()) l.remove(i.next());
		if(l.size() == vs.length) return;
		vs = (String[])l.toArray(new String[0]);
		selectedTemplate.getInterfaces().setValues(vs);
		interfacesAdapter.setValue(vs);
	}

	private void editInterface() {
		if(selectedTemplate == null) return;
		ISelection s = interfacesAdapter.getSelection();
		if(s.isEmpty()) return;
		String current = ((StructuredSelection)s).getFirstElement().toString();
		ArrayList<String> l = new ArrayList<String>();
		String[] vs = selectedTemplate.getInterfaces().getValues();
		if(vs == null) vs = new String[0];
		for (int i = 0; i < vs.length; i++) l.add(vs[i]);
		int index = l.indexOf(current);		
		String v = AddInterfaceSupport.runEdit(model, current);
		if(v == null || v.equals(current)) return;
		l.set(index, v);
		vs = (String[])l.toArray(new String[0]);
		selectedTemplate.getInterfaces().setValues(vs);
		interfacesAdapter.setValue(selectedTemplate.getInterfaces().getValues());
	}

    private Control[] getControls(Composite composite, PropertyEditor editor) {
        return ((IFieldEditor)editor.getFieldEditor(composite)).getControls(composite);
    }

    public void setSelectedTemplate(MetaClassTemplate selectedTemplate) {
    	this.selectedTemplate = selectedTemplate;
    	if(selectedTemplate == null) {
    	    if(xPathLabelValue!=null) {
    	        xPathLabelValue.setText("");
    	    } else {
    	        initValue = "";
    	    }
    		baseClassAdapter.value = null;
    		baseClassAdapterListener.value = null;
    		interfacesAdapter.setValue(null);
    	} else {
    	    if(xPathLabelValue!=null) {
    	        xPathLabelValue.setText(selectedTemplate.getAxis());
    	    } else {
    	        initValue = selectedTemplate.getAxis();
    	    }
        	MetaValue v = selectedTemplate.getSuperClass();
        	baseClassAdapterListener.value = null;
        	baseClassAdapter.value = v;
        	baseClassAdapter.load();
        	baseClassAdapterListener.value = v;
        	interfacesAdapter.setValue(selectedTemplate.getInterfaces().getValues());
    	}
    	boolean enabled = (selectedTemplate != null);
		if(composite != null) {
			baseClassEditor.getFieldEditor(composite).setEnabled(enabled);
			interfacesEditor.getFieldEditor(composite).setEnabled(enabled);
		}
    }
    
    class ActionProvider extends CompositeActionProvider {
    	IAction addAction;
    	IAction removeAction;
    	IAction editAction;
    	
    	public ActionProvider() {
    		addAction = new Action("Add") {
    	    	public void run() {
    	    	    addInterface();
    	    	}
    	    };
    		removeAction = new Action("Remove") {
    	    	public void run() {
    	    	    removeInterface();
    	    	}
    	    };
    		editAction = new Action("Edit") {
    	    	public void run() {
    	    	    editInterface();
    	    	}
    	    };
    		addAction(addAction);
    		addAction(removeAction);
    		addAction(editAction);
    	}

		public void update(ISelection selection) {
			removeAction.setEnabled(!selection.isEmpty());
			editAction.setEnabled(!selection.isEmpty());
		}    	
    }

}