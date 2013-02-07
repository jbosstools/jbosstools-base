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

import java.util.HashMap;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.options.SharableConstants;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class PaletteInsertManager {
	static PaletteInsertManager instance = new PaletteInsertManager();
	
	public static PaletteInsertManager getInstance() {
		return instance;
	}
	
	HashMap<String,IConfigurationElement> tagWizards = null;
	
	public String getWizardName(Properties properties) {
		IConfigurationElement o = getElement(properties);
		return o != null ? o.getAttribute("class") : null; //$NON-NLS-1$
	}
	
	public Object createWizardInstance(Properties properties) {
		IConfigurationElement o = getElement(properties);
		try {
			return o.createExecutableExtension("class"); //$NON-NLS-1$
		} catch(CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return null;
		}
	}

	private IConfigurationElement getElement(Properties properties) {
		if(tagWizards == null) {
			loadWizards();
		}
		String palettePath = properties.getProperty(SharableConstants.PALETTE_PATH);
		palettePath = palettePath.replace('%', '_').replace(' ', '_');
		IConfigurationElement result = tagWizards.get(palettePath);
		if(result == null) {
			String tagname = properties.getProperty("tag name"); //$NON-NLS-1$
			result = tagWizards.get(tagname);
		}
		return result;
	}
	
	private void loadWizards() {
		tagWizards = new HashMap<String,IConfigurationElement>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint("org.jboss.tools.common.model.ui.InsertTagWizard"); //$NON-NLS-1$
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String name = elements[j].getAttribute("name"); //$NON-NLS-1$
				String className = elements[j].getAttribute("class"); //$NON-NLS-1$
				if(name != null && name.trim().length() > 0 && className != null && className.trim().length() > 0) {
					tagWizards.put(name.trim(), elements[j]);
				}
			}
		}		
	}

}
