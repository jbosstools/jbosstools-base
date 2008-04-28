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

import java.util.HashMap;
import org.eclipse.core.resources.IProject;

import org.jboss.tools.common.model.ui.templates.model.MetaElementFactory;

public class MetaConfigurationManager {
	static MetaConfigurationManager instance = new MetaConfigurationManager();
	
	public static MetaConfigurationManager getInstance() {
		return instance;
	}

	private MetaConfigurationLoader loader = new MetaConfigurationLoader();

	private MetaConfiguration extensionConfiguration;
	private MetaConfiguration globalConfiguration;
    private HashMap<String,MetaConfiguration> projectConfigurations = new HashMap<String,MetaConfiguration>();
    
    public MetaConfiguration getExtensionConfiguration() {
    	if(extensionConfiguration == null) {
    		extensionConfiguration = createExtensionConfiguration();
    	}
    	return extensionConfiguration;
    }
    
    public MetaConfiguration getGlobalConfiguration() {
    	if(globalConfiguration == null) {
    		globalConfiguration = createGlobalConfiguration();
    	}
    	return globalConfiguration;
    }
    
    public MetaConfiguration getProjectConfiguration(IProject project) {
    	MetaConfiguration projectConfiguration = (MetaConfiguration)projectConfigurations.get(project.getName());
    	if(projectConfiguration == null) {
    		projectConfiguration = createProjectConfiguration(project);
    		projectConfigurations.put(project.getName(), projectConfiguration);
    	}
    	return projectConfiguration;
    }
    
    public MetaConfiguration getWorkingCopy(final MetaConfiguration source) {
    	final MetaConfiguration c = MetaElementFactory.instance.createConfiguraton(source);
    	c.setSaveAgent(new IMetaConfigurationSave() {
    		public void save() {
    			c.commitToParent();
    			source.save();
    		}
    	});
    	return c;
    }
    
    private MetaConfiguration createExtensionConfiguration() {
    	MetaConfiguration c = MetaElementFactory.instance.createConfiguraton(null);
    	loader.loadExtensionConfiguration(c);
    	return c;
    }
    
    private MetaConfiguration createGlobalConfiguration() {
    	final MetaConfiguration c = MetaElementFactory.instance.createConfiguraton(getExtensionConfiguration());
    	loader.loadGlobalConfiguration(c);
    	c.setSaveAgent(new IMetaConfigurationSave() {
    		public void save() {
    			loader.saveGlobalConfiguration(c);
    		}
    	});
    	return c;
    }
    
    private MetaConfiguration createProjectConfiguration(final IProject p) {
    	final MetaConfiguration c = MetaElementFactory.instance.createConfiguraton(getGlobalConfiguration());
    	loader.loadProjectConfiguration(c, p);
    	c.setSaveAgent(new IMetaConfigurationSave() {
    		public void save() {
    			loader.saveProjectConfiguration(c, p);
    		}
    	});
    	return c;
    }
    
    public void saveGlobalConfiguration() {
    	loader.saveGlobalConfiguration(globalConfiguration);
    }
	
    public void saveProjectConfiguration(IProject p) {
    	loader.saveProjectConfiguration(getProjectConfiguration(p), p);
    }
	
}
