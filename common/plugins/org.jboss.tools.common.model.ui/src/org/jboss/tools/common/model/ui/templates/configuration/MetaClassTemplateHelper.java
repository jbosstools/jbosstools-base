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
package org.jboss.tools.common.model.ui.templates.configuration;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.QualifiedName;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.templates.model.*;

public class MetaClassTemplateHelper {
	public static MetaClassTemplateHelper instance = new MetaClassTemplateHelper();
	
	public MetaConfiguration getCurrentConfiguration(IProject project) {
		MetaConfigurationManager m = MetaConfigurationManager.getInstance();
		if(isProjectOverrideTemplates()) {
			return m.getProjectConfiguration(project);
		} else {
			return m.getGlobalConfiguration();
		}
		
	}

	public MetaClassTemplate getMetaTemplate(IProject project, String publicId, String axis) {
		MetaClassTemplate template = null;
		if (project != null) {
			MetaConfiguration configuration = getCurrentConfiguration(project);
			if (configuration != null) {
				template = configuration.getMetaTemplate(publicId, axis);
			}
		}
		return template;
	}
    
	public String getSuperClassName(IProject project, String publicId, String axis) {
		MetaClassTemplate template = getMetaTemplate(project, publicId, axis);
		return (template != null) ? template.getSuperClass().getValue() : null;
	}

	public String[] getInterfacesName(IProject project, String publicId, String axis) {
		MetaClassTemplate template = getMetaTemplate(project, publicId, axis);
		return (template != null) ? template.getInterfaces().getValues() : null;
	}

	public boolean isProjectOverrideTemplates() {
		QualifiedName qn = new QualifiedName(ModelUIPlugin.ID_PLUGIN, ModelUIPlugin.PROJECT_OVERRIDE); 
		try {
			String projectOverride = ResourcesPlugin.getWorkspace().getRoot().getPersistentProperty(qn);
			return projectOverride != null && Boolean.valueOf(projectOverride).booleanValue();
		} catch (Exception e) {
			return false;
		}
    }
	
	public void setProjectOverrideTemplates(boolean b) {
		QualifiedName qn = new QualifiedName(ModelUIPlugin.ID_PLUGIN, ModelUIPlugin.PROJECT_OVERRIDE); 
		try {
			ResourcesPlugin.getWorkspace().getRoot().setPersistentProperty(qn, "" + b);
		} catch (Exception e) {
		}
	}

}
