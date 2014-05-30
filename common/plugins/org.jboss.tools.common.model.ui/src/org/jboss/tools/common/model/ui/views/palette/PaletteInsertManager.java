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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.options.SharableConstants;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class PaletteInsertManager {
	static PaletteInsertManager instance = new PaletteInsertManager();

	static String POINT_ID = "org.jboss.tools.common.model.ui.InsertTagWizard"; //$NON-NLS-1$
	static String ATTR_CORRECTOR_CLASS = "corrector-class"; //$NON-NLS-1$
	
	public static PaletteInsertManager getInstance() {
		return instance;
	}
	
	HashMap<String,IConfigurationElement> tagWizards = null;
	Map<String, List<String>> keywords = null;
	
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

	public IPositionCorrector createCorrectorInstance(String palettePath) {
		IConfigurationElement o = getElement(palettePath, null);
		if(o != null) {
			String correctorClassName = o.getAttribute(ATTR_CORRECTOR_CLASS);
			try {
				if(correctorClassName != null) {
					return (IPositionCorrector)o.createExecutableExtension(ATTR_CORRECTOR_CLASS);
				}
			} catch(CoreException e) {
				ModelUIPlugin.getPluginLog().logError(e);
				return null;
			}
		}
		return null;
	}

	/**
	 * Returns list of logic names by which palette item can be invoked.
	 * 
	 * @param palettePath
	 * @return
	 */
	public List<String> getKeyWords(String palettePath) {
		if(keywords == null) {
			loadKeywords();
		}
		palettePath = palettePath.replace('%', '_').replace(' ', '_');
		return keywords.get(palettePath);
	}

	private IConfigurationElement getElement(Properties properties) {
		String palettePath = properties.getProperty(SharableConstants.PALETTE_PATH);
		return getElement(palettePath, properties.getProperty("tag name"));
	}

	private IConfigurationElement getElement(String palettePath, String tagName) {
		if(tagWizards == null) {
			loadWizards();
		}
		palettePath = palettePath.replace('%', '_').replace(' ', '_');
		IConfigurationElement result = tagWizards.get(palettePath);
		if(result == null && tagName != null) {
			result = tagWizards.get(tagName);
		}
		return result;
	}
	
	private void loadWizards() {
		tagWizards = new HashMap<String,IConfigurationElement>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(POINT_ID);
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

	private void loadKeywords() {
		keywords = new HashMap<String, List<String>>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(POINT_ID);
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String name = elements[j].getAttribute("name"); //$NON-NLS-1$
				String keywordString = elements[j].getAttribute("keywords"); //$NON-NLS-1$
				if(keywordString != null) {
					String[] ks = keywordString.split(",");
					if(ks.length > 0) {
						List<String> list = new ArrayList<String>();
						for (String k: ks) list.add(k);
						keywords.put(name, list);
					}
				}				
			}
		}		
	}

}
