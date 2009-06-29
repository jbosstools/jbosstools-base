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
package org.jboss.tools.common.model.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.InvalidRegistryObjectException;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.osgi.framework.Bundle;


/**
 * TODO use IExecutableExtension and IExecutableExtensionFactory
 * 
 * @author Slava kabanovich
 *
 */
public class ModelFeatureFactory {
	public static final String POINT = "org.jboss.tools.common.model.classes"; //$NON-NLS-1$
	
	public static ModelFeatureFactory getInstance() {
		return ModelFeatureFactoryHolder.INSTANCE;
	}
	
	private Map<String,IConfigurationElement> elements = new HashMap<String, IConfigurationElement>();
	private Set<String> instanceFailures = new HashSet<String>();
	private Set<String> classFailures = new HashSet<String>();
	
	private ModelFeatureFactory() {
		init();
	}
	
	private void init() {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(POINT);
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] cs = es[i].getConfigurationElements();
			for (int j = 0; j < cs.length; j++) {
				String id = cs[j].getAttribute("id"); //$NON-NLS-1$
				elements.put(id, cs[j]);
			}
		}
	}
	
	private boolean isActive() {
		Bundle b = Platform.getBundle("org.jboss.tools.common.model"); //$NON-NLS-1$
		int state = b == null ? -1 : b.getState() ;
		return state == Bundle.ACTIVE;
	}
	
	public Object createFeatureInstance(String id) {
		if(instanceFailures.contains(id)) return null;
		IConfigurationElement c = elements.get(id);
		if(c == null) {
			instanceFailures.add(id);
			classFailures.add(id);
			ModelPlugin.getPluginLog().logError(new Exception("Model feature " + id + " is not registered with extension point " + POINT)); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		try {
			return c.createExecutableExtension("class"); //$NON-NLS-1$
		} catch (CoreException e) {
			instanceFailures.add(id);
			ModelPlugin.getPluginLog().logError("Cannot create model feature instance " + id + ".", e); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (InvalidRegistryObjectException e) {
			instanceFailures.add(id);
			ModelPlugin.getPluginLog().logError("Cannot create model feature instance " + id + ".", e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return null;
	}
	
	public Class getFeatureClass(String id) {
		if(classFailures.contains(id)) return null;
		IConfigurationElement c = elements.get(id);
		if(c == null) {
			classFailures.add(id);
			instanceFailures.add(id);
			ModelPlugin.getPluginLog().logError(new Exception("Model feature " + id + " is not registered with extension point " + POINT)); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		try {
			String n = c.getNamespaceIdentifier();
			Bundle b = Platform.getBundle(n);
			String cls = c.getAttribute("class"); //$NON-NLS-1$
			return b.loadClass(cls);
		} catch (ClassNotFoundException e) {
			classFailures.add(id);
			if(!isActive()) return null;
			ModelPlugin.getPluginLog().logError("Cannot create model feature class " + c.getAttribute("class") + ".", e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return null;
	}

	public XModelObject createXModelObjectInstance(String id) {
		Object o = createFeatureInstance(id);
		if(o == null) return null;
		try {
			return (XModelObject)o;
		} catch (ClassCastException e) {
			instanceFailures.add(id);
			ModelPlugin.getPluginLog().logError("Model feature " + id + " is not instance of XModelObject", e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}
	
	public static class ModelFeatureFactoryHolder {
		public static final ModelFeatureFactory INSTANCE = new ModelFeatureFactory();
	}

}
