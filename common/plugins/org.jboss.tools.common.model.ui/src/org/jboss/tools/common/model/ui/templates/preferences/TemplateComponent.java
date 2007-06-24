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

import java.util.*;

import org.jboss.tools.common.model.ui.attribute.adapter.*;
import org.jboss.tools.common.model.ui.attribute.editor.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.ui.templates.configuration.*;
import org.jboss.tools.common.model.ui.templates.model.*;

public class TemplateComponent {

	private static final String RESOURCE_BUNDLE= "org.jboss.tools.common.model.ui.templates.preferences.messages";
	private static ResourceBundle resourceBundle= ResourceBundle.getBundle(RESOURCE_BUNDLE);
	private MetaConfiguration configuration;
//    private XModel model;
    boolean isGlobal = true;
    // allow override
    CheckBoxEditor allowOverrideEditor;
	CheckBoxEditor qwe;
	DefaultValueAdapter allowOverrideAdapter;
    // template groups
    TableStructuredEditor templateGroupsEditor;
    DefaultTableStructuredAdapter templateGroupsAdapter;
    // axis
    TableStructuredEditor axisEditor;
    DefaultTableStructuredAdapter axisAdapter;
    
    ClassTemplateComponent templateComponent = new ClassTemplateComponent();
    
    public void setGlobal(boolean b) {
    	isGlobal = b;
    }
    
    private Control[] getControls(Composite composite, PropertyEditor editor) {
        return ((IFieldEditor)editor.getFieldEditor(composite)).getControls(composite);
    }
    
    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout gridLayout = new GridLayout(3, Boolean.FALSE.booleanValue());
        composite.setLayout(gridLayout);
        
        GridData gd;
        Control[] control;
        // checkbox
        if(!isGlobal) {  //.getString CHECKBOX_LABEL
        	allowOverrideEditor.setLabelText(resourceBundle.getString("CHECKBOX_LABEL"));
        	allowOverrideAdapter.setValue("" + MetaClassTemplateHelper.instance. isProjectOverrideTemplates());
            control = getControls(composite, allowOverrideEditor);
            control[0].dispose(); // dispose empty label
            gd = new GridData(GridData.FILL_HORIZONTAL);
            gd.horizontalSpan = 3;
            control[1].setLayoutData(gd);
        }
      
        // list of templates
        templateGroupsEditor.setLabelText("List of template groups.");
        control = getControls(composite, templateGroupsEditor);
        control[0].dispose(); // dispose empty label
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 3;
        gd.heightHint = 100;
        control[1].setLayoutData(gd);
        
        // axis
        axisEditor.setLabelText("Axis.");
        control = getControls(composite, axisEditor);
        control[0].dispose(); // dispose empty label
        gd = new GridData(GridData.FILL_VERTICAL);
        gd.widthHint = 150;
        gd.verticalSpan = 3;
        control[1].setLayoutData(gd);
        
        templateComponent.createContents(composite);

        return composite;
    }
    
	public void setConfiguration(MetaConfiguration configuration, XModel model) {
	    this.configuration = configuration;
//	    this.model = model;
   
	    // allow override
	   if(!isGlobal) {
	    	allowOverrideEditor = new CheckBoxEditor();
	    	allowOverrideAdapter = new DefaultValueAdapter();
	    	allowOverrideEditor.setInput(allowOverrideAdapter);
	    }
	    // template groups
	    templateGroupsEditor = new TableStructuredEditor();
	    templateGroupsAdapter = new DefaultTableStructuredAdapter();
	    templateGroupsAdapter.addColumnDescription(new ColumnDescription("Groups", null, 100, SWT.LEFT, true, null));
	    Collection col = configuration.getMetaTemplateGroups();
	    templateGroupsAdapter.setValue(col.toArray());
	    templateGroupsAdapter.setTableLabelProvider(new TableLabelProvider());
	    templateGroupsAdapter.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
        		if (event.getSelection() instanceof StructuredSelection) {
        			StructuredSelection structuredSelection = (StructuredSelection)event.getSelection();
        			Object object = structuredSelection.getFirstElement();
       			    setSelection(object);
        		}
            }
	    });
	    templateGroupsEditor.setInput(templateGroupsAdapter);
	    
	    // axis
	    axisEditor = new TableStructuredEditor();
	    axisAdapter = new DefaultTableStructuredAdapter();
	    axisAdapter.addColumnDescription(new ColumnDescription("Axis", null, 100, SWT.LEFT, true, null));
	    axisAdapter.setTableLabelProvider(new TableLabelProvider());
	    axisAdapter.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
        		if (event.getSelection() instanceof StructuredSelection) {
        			StructuredSelection structuredSelection = (StructuredSelection)event.getSelection();
        			Object object = structuredSelection.getFirstElement();
       			    setSelection(object);
        		}
            }
	    });
	    axisEditor.setInput(axisAdapter);
	    
	    templateComponent.setModel(model);
	}

	private void setSelection(Object o) {
		if(o == null) {
			templateComponent.setSelectedTemplate(null);
		} else if (o instanceof MetaGroup) {
            axisAdapter.setValue(((MetaGroup)o).getTemplates().toArray());
        } else if (o instanceof MetaClassTemplate) {
        	templateComponent.setSelectedTemplate((MetaClassTemplate)o);
        }
	}
	
    protected void performApply() {
    	if(!isGlobal) {
    		String s = allowOverrideAdapter.getValue().toString();
    		MetaClassTemplateHelper.instance.setProjectOverrideTemplates("true".equalsIgnoreCase(s));
    	}
    	configuration.save();
    }
    
	protected void performDefaults() {
		configuration.loadFromParent(2);
		if(templateComponent != null) templateComponent.setSelectedTemplate(templateComponent.selectedTemplate);
	}
}
