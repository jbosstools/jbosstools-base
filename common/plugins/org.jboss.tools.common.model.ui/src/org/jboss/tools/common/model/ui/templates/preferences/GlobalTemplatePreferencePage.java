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

import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.common.model.ui.templates.configuration.*;

/**
 * @author au
 */
public class GlobalTemplatePreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
    TemplateComponent templateComponent = new TemplateComponent();

    protected Control createContents(Composite parent) {
    	return templateComponent.createContents(parent);
    }
    
	public void init(IWorkbench workbench) {
		MetaConfiguration source = MetaConfigurationManager.getInstance().getGlobalConfiguration();
		MetaConfiguration copy = MetaConfigurationManager.getInstance().getWorkingCopy(source);
		templateComponent.setConfiguration(copy, ModelUtilities.getPreferenceModel());	    
	}
	
    protected void performApply() {
    	templateComponent.performApply();
    }
    
    public boolean performOk() {
    	templateComponent.performApply();
    	return super.performOk();
    }

	protected void performDefaults() {
		templateComponent.performDefaults();
	}

}
