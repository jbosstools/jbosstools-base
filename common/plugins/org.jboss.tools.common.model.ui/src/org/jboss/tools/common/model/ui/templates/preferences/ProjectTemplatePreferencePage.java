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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.PropertyPage;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.templates.configuration.*;

/**
 * @author au
 */
public class ProjectTemplatePreferencePage extends PropertyPage {
    TemplateComponent templateComponent = new TemplateComponent();
    
    public ProjectTemplatePreferencePage() {
    	templateComponent.setGlobal(false);
    }

    protected Control createContents(Composite parent) {
    	return templateComponent.createContents(parent);
    }
    
    protected void performApply() {
    	templateComponent.performApply();
    }

    public boolean performOk() {
    	templateComponent.performApply();
    	return super.performOk();
    }

    public void setElement(IAdaptable element) {
        if (element instanceof IProject) {
        	IProject project = (IProject)element;
        	IModelNature n = EclipseResourceUtil.getModelNature(project);
        	XModel model = (n == null) ? ModelUtilities.getPreferenceModel() : n.getModel();
    		MetaConfiguration source = MetaConfigurationManager.getInstance().getProjectConfiguration(project);
    		MetaConfiguration copy = MetaConfigurationManager.getInstance().getWorkingCopy(source);
        	templateComponent.setConfiguration(copy, model);
        }
        super.setElement(element);
    }

}
