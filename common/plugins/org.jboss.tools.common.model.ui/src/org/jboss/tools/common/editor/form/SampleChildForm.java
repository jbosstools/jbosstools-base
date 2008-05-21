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
package org.jboss.tools.common.editor.form;

import org.jboss.tools.common.model.ui.attribute.adapter.XChildrenTableStructuredAdapter;
import org.jboss.tools.common.model.ui.attribute.editor.IFieldEditor;
import org.jboss.tools.common.model.ui.attribute.editor.TableStructuredEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.forms.ExpandableForm;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.WhiteSettings;

public class SampleChildForm extends ExpandableForm {
	private XModelObject xmo;
//	private XModel model;
	
	private IWidgetSettings settings = new WhiteSettings();

	private TableStructuredEditor tableEditor;
	private XChildrenTableStructuredAdapter tableAdapter;



	private static final String FORM_DESCRIPTION = "SampleChildForm.description";
	private static final String FORM_HEADER = "SampleChildForm.header";

	public SampleChildForm() {}

	public void dispose() {
		super.dispose();
		if (tableEditor!=null) tableEditor.dispose();
		tableEditor = null;
		if (tableAdapter!=null) tableAdapter.dispose();
		tableAdapter = null;
	}

	protected Control  createClientArea(Composite parent, IWidgetSettings factory) {
		Composite composite = new Composite(parent, SWT.NONE);
		settings.setupControl(composite);
		GridLayout layout = new GridLayout(2,Boolean.FALSE.booleanValue());
		
		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 5;
		layout.marginHeight = 5;
		layout.marginWidth = 5;
		composite.setLayout(layout);
		Control[] control;
		GridData gd;
		
		Label label = new Label(composite, SWT.WRAP);
		settings.setupControl(label);
		label.setText(FORM_DESCRIPTION);
		gd = new GridData();
		gd.horizontalSpan = 2;
		label.setLayoutData(gd);

		control = ((IFieldEditor)tableEditor.getFieldEditor(composite)).getControls(composite);

		control[0].dispose(); // cannot show label

		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = 2;
		control[1].setLayoutData(gd);
		
		composite.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				dispose();
			}
		});
		
		return composite;
	}

	public void initialize(Object model) {
		this.xmo = (XModelObject)model;
		this.model = xmo.getModel();
		//this.support.init(xmo);
		//this.support.setAutoStore(Boolean.TRUE.booleanValue());
		this.setHeadingText(FORM_HEADER);
		
		this.tableAdapter = new XChildrenTableStructuredAdapter();
		//this.tableAdapter.setShownEntities(new String[] {"XXXXX"});
		this.tableAdapter.getActionMapping().clear();
		this.tableAdapter.getActionMapping().put(TableStructuredEditor.ADD_ACTION,"CreateActions.CreateFilter");
		this.tableAdapter.getActionMapping().put(TableStructuredEditor.REMOVE_ACTION,"DeleteActions.Delete");
		this.tableAdapter.getActionMapping().put(TableStructuredEditor.EDIT_ACTION,"Properties.Properties");
		this.tableAdapter.getActionMapping().put(TableStructuredEditor.UP_ACTION,"%internal%");
		this.tableAdapter.getActionMapping().put(TableStructuredEditor.DOWN_ACTION,"%internal%");
		this.tableAdapter.setShownProperties(new String[] {"name"});
		this.tableAdapter.setColumnLabels(new String[] {"Name"});
		this.tableAdapter.setWidths(new int[] {100});
		this.tableAdapter.setModelObject(xmo);
		
		this.tableEditor = new TableStructuredEditor(settings);
		this.tableEditor.setLabelText("");
		this.tableEditor.setInput(this.tableAdapter); 
	}
	
	public void update() {
	}

	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		if (tableEditor!=null) {
			tableEditor.getFieldEditor(null).setEnabled(enabled);
		}
	}
}