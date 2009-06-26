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
package org.jboss.tools.common.model.ui.objecteditor;

import java.util.Properties;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizard;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;

public class PropertiesWizard extends AbstractQueryWizard {

	public PropertiesWizard() {
		setView(new PropertiesWizardView());
		getView().setTitle("Properties");
	}
}

class PropertiesWizardView extends AbstractQueryWizardView {
	private XModelObjectEditor objectEditor = new XModelObjectEditor();
	private Object dataObject = null;

	public PropertiesWizardView() {
	}

	public String[] getCommands() {
		return new String[]{CLOSE, HELP};
	}

	public String getDefaultCommand() {
		return CLOSE;
	}

	public void setObject(Object data) {
		super.setObject(data);
		//this.setMessage(WizardKeys.getString(getHelpKey()+".Message"));
		Object[] ds = (Object[])data;
		dataObject = ds[0];
		objectEditor.setModelObject((XModelObject)dataObject);
		Properties p = findProperties(data);
		if(windowTitle == null) {
			windowTitle = WizardKeys.getHeader("Properties"); //$NON-NLS-1$
		}
		boolean viewMode = p != null && "true".equals(p.getProperty("viewMode")); //$NON-NLS-1$ //$NON-NLS-2$
		objectEditor.setViewMode(viewMode);
	}

	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);

		return objectEditor.createControl(composite);
	}
	
	public void stopEditing() {
		if(objectEditor != null) objectEditor.stopEditing();
	}
	
	public void dispose() {
		super.dispose();
		if (objectEditor!=null) {
			objectEditor.setModelObject(null);
			objectEditor.dispose();
		}
	}

	public void action(String command) {
		stopEditing();
		if(CLOSE.equals(command)) {
			setCode(0);
			dispose();
		}
		else super.action(command);
	}

}
