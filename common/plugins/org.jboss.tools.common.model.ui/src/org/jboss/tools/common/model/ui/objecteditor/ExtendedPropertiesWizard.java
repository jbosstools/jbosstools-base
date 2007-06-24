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

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.wizards.query.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;

public class ExtendedPropertiesWizard extends AbstractQueryWizard {
	
	public static void run(ExtendedProperties attributes) {
		ExtendedPropertiesWizard wizard = new ExtendedPropertiesWizard();
		Properties p = new Properties();
		p.put("extendedProperties", attributes);
		p.put("model", PreferenceModelUtilities.getPreferenceModel());		
		wizard.setObject(p);
		wizard.execute();
	}

	public ExtendedPropertiesWizard() {
		setView(new ExtendedPropertiesWizardView());
	}
}

class ExtendedPropertiesWizardView extends AbstractQueryWizardView {
	private ExtendedPropertiesEditor objectEditor = new ExtendedPropertiesEditor();

	public ExtendedPropertiesWizardView() {}

	public String[] getCommands() {
		return new String[]{CLOSE, HELP};
	}

	public String getDefaultCommand() {
		return CLOSE;
	}

	public void setObject(Object data) {
		super.setObject(data);
		try {
			Properties p = findProperties(data);
			ExtendedProperties attributes = (ExtendedProperties)p.get("extendedProperties");
			objectEditor.setExtendedProperties(attributes);
			boolean viewMode = p != null && "true".equals(p.getProperty("viewMode"));
			objectEditor.setReadOnly(viewMode);
			setWindowTitle("Attributes");
			String nodeName = attributes.getNodeName();
			setTitle((nodeName != null) ? "<" + nodeName + ">" : "");
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
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
		try { 
			objectEditor.stopEditing();
		} catch (Exception e) {}
	}
	
	public void dispose() {
		super.dispose();
		if (objectEditor != null) {
			objectEditor.setExtendedProperties(null);
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
