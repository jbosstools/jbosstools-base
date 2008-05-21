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
package org.jboss.tools.common.model.adapter;

import java.util.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.osgi.framework.Bundle;

public class ModelObjectAdapterExtensionPoint {
	static String POINT_ID = "org.jboss.tools.common.model.modelObjectAdapter";
	static ModelObjectAdapterExtensionPoint instance;

	IExtensionPoint point = null;
	Map<String,Class> adapters = new HashMap<String,Class>();
	
	public static ModelObjectAdapterExtensionPoint getInstance() {
		if(instance == null) {
			instance = new ModelObjectAdapterExtensionPoint();
			instance.init();
		}
		return instance;
	}
	
	private ModelObjectAdapterExtensionPoint() {}
	
	private void init() {
		point = Platform.getExtensionRegistry().getExtensionPoint(POINT_ID);
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] cs = es[i].getConfigurationElements();
			for (int j = 0; j < cs.length; j++) {
				Bundle bundle = Platform.getBundle(es[i].getNamespaceIdentifier());
				String iclassname = cs[j].getAttribute("iclass");
				String classname = cs[j].getAttribute("class");
				Class cls = null;
				try {
					cls = bundle.loadClass(classname);
				} catch (Exception e) {
					ModelPlugin.getPluginLog().logError("Cannot load editor class " + classname + " from " + es[i].getNamespaceIdentifier());
					continue;
				}
				if(!IModelObjectAdapter.class.isAssignableFrom(cls)) {
					ModelPlugin.getPluginLog().logInfo("Class " + classname + " must implement IModelObjectAdapter");
				} else {
					adapters.put(iclassname, cls);
				}
			}
		}
	}
	
	public IModelObjectAdapter getAdapter(String iclassname) {
		Class cls = (Class)adapters.get(iclassname);
		try {
			if(cls != null) return (IModelObjectAdapter)cls.newInstance();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return null;
	}

}
