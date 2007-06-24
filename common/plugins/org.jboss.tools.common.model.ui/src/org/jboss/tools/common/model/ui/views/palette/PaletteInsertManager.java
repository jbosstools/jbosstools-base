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
package org.jboss.tools.common.model.ui.views.palette;

import java.util.*;
import org.eclipse.core.runtime.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.jst.web.tld.URIConstants;

public class PaletteInsertManager {
	static PaletteInsertManager instance = new PaletteInsertManager();
	
	public static PaletteInsertManager getInstance() {
		return instance;
	}
	
	HashMap<String,IConfigurationElement> tagWizards = null;
	
	public String getWizardName(Properties properties) {
		String tagname = properties.getProperty("tag name");
		String uri = properties.getProperty(URIConstants.LIBRARY_URI);
		return getWizardName(tagname, uri);
	}
	
	public Object createWizardInstance(Properties properties) {
		String tagname = properties.getProperty("tag name");
		if(tagWizards == null) {
			loadWizards();
		}
		if(tagname == null) return null;
		IConfigurationElement o = tagWizards.get(tagname);
		try {
			return o.createExecutableExtension("class");
		} catch(CoreException e) {
			ModelPlugin.log(e);
			return null;
		}
	}
	
	private String getWizardName(String tagname, String uri) {
		if(tagWizards == null) {
			loadWizards();
		}
		if(tagname == null) return null;
		IConfigurationElement o = tagWizards.get(tagname);
		return o != null ? o.getAttribute("class") : null;
	}
	
	private void loadWizards() {
		tagWizards = new HashMap<String,IConfigurationElement>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint("org.jboss.tools.common.model.ui.InsertTagWizard");
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String name = elements[j].getAttribute("name");
				String className = elements[j].getAttribute("class");
				if(name != null && name.trim().length() > 0 && className != null && className.trim().length() > 0) {
					tagWizards.put(name.trim(), elements[j]);
				}
			}
		}		
	}

}
