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
package org.jboss.tools.common.text.xml;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.ui.text.JavaTextTools;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.wst.xml.ui.internal.XMLUIPlugin;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.jst.jsp.preferences.xpl.PreferenceConstants;
import org.jboss.tools.jst.jsp.preferences.xpl.XMLOccurrencePreferenceConstants;
import org.jboss.tools.jst.jsp.preferences.xpl.XMLOccurrencesPreferencePage;
import org.jboss.tools.jst.jsp.text.xpl.StructuredTextOccurrenceStructureProviderRegistry;

/**
 * The main plugin class to be used in the desktop.
 */
public class XmlEditorPlugin extends BaseUIPlugin {
	//The shared instance.
	private static XmlEditorPlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	
	public static final String PLUGIN_ID = "org.jboss.tools.common.text.xml";  //$NON-NLS-1$

	
	/**
	 * The constructor.
	 */
	public XmlEditorPlugin() {
		plugin = this;
		try {
			resourceBundle= ResourceBundle.getBundle("org.jboss.tools.common.text.xml.XmlEditorPluginResources"); //$NON-NLS-1$
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
		initDefaultPluginPreferences();
	}

	/**
	 * Returns the shared instance.
	 */
	public static XmlEditorPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle= XmlEditorPlugin.getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}
	
	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	protected void initializeDefaultPluginPreferences() {
		super.initializeDefaultPluginPreferences();

//		XmlPreferenceConstants.initializeDefaultValues(getPreferenceStore());

			Method m;
			try {
				m = AbstractUIPlugin.class.getDeclaredMethod("initializeDefaultPluginPreferences", new Class[0]); //$NON-NLS-1$
				m.setAccessible(true);
				m.invoke(JavaPlugin.getDefault(), new Object[0]);
			} catch (NoSuchMethodException e) {
			} catch (IllegalArgumentException e) {
			} catch (IllegalAccessException e) {
			} catch (InvocationTargetException e) {
			}


		IPreferenceStore store = getPreferenceStore();
		
		PreferenceConstants.initializeDefaultValues(store);
		//New Text Editors 
		//JspPreferenceConstants.initializeDefaultValues(store);
//		XmlPreferenceConstants.initializeDefaultValues(store);
		
		(new XMLOccurrencesPreferencePage()).initializeDefaultValues();

	}


	public void initDefaultPluginPreferences() {
		IPreferenceStore store = XMLUIPlugin.getDefault().getPreferenceStore();
		
		PreferenceConstants.initializeDefaultValues(store);
		XMLOccurrencePreferenceConstants.initializeDefaultValues(store);

	}

	/* New Text Editors */	
	private JavaTextTools fJavaTextTools;

	public synchronized JavaTextTools getJavaTextTools() {
		if (fJavaTextTools == null)
			fJavaTextTools= new JavaTextTools(getPreferenceStore());
		return fJavaTextTools;
	}
	/* New Text Editors */	

	/**
	 * The extension point registry for the <code>org.eclipse.jdt.ui.javaFoldingStructureProvider</code>
	 * extension point.
	 * 
	 * @since 3.0
	 */
	private Map fOccurenceStructureProviderRegistry;

	/**
	 * Returns the registry of the extensions to the <code>org.eclipse.jdt.ui.javaFoldingStructureProvider</code>
	 * extension point.
	 * 
	 * @return the registry of contributed <code>IJavaFoldingStructureProvider</code>
	 * @since 3.0
	 */
	public synchronized StructuredTextOccurrenceStructureProviderRegistry getOccurrenceStructureProviderRegistry(String fEditorId) {
		if (fOccurenceStructureProviderRegistry == null) {
			fOccurenceStructureProviderRegistry = new HashMap();
		}
		if (fOccurenceStructureProviderRegistry.get(fEditorId) == null) {
			fOccurenceStructureProviderRegistry.put(fEditorId, new StructuredTextOccurrenceStructureProviderRegistry(fEditorId));
		}
		
		return (StructuredTextOccurrenceStructureProviderRegistry)fOccurenceStructureProviderRegistry.get(fEditorId);
	}


	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.log.BaseUIPlugin#getId()
	 */
	@Override
	public String getId() {
		return PLUGIN_ID;
	}
}