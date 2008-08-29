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
package org.jboss.tools.common.model.ui.editor;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.osgi.framework.Bundle;

public class EditorPartWrapperExtension {
	static String POINT_ID = "org.jboss.tools.common.model.ui.xmlEditor";
	static EditorPartWrapperExtension instance;
	Map<String,EditorPartFactory> factories = new HashMap<String,EditorPartFactory>();
	Map<String,Integer> priorities = new HashMap<String,Integer>();
	 
	IExtensionPoint point = null;
	
	public static EditorPartWrapperExtension getInstance() {
		if(instance == null) {
			instance = new EditorPartWrapperExtension();
			instance.init();
		}
		return instance;
	}
	
	private EditorPartWrapperExtension() {}
	
	private void init() {
		point = Platform.getExtensionRegistry().getExtensionPoint(POINT_ID);
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] cs = es[i].getConfigurationElements();
			for (int j = 0; j < cs.length; j++) {
				Bundle bundle = Platform.getBundle(es[i].getNamespaceIdentifier());
				String entities = cs[j].getAttribute("entities");
				String editor = cs[j].getAttribute("class");
				String priorityString = cs[j].getAttribute("priority");
				int priority = 0;
				try {
					if(priorityString != null && priorityString.length() > 0) {
						priority = Integer.parseInt(priorityString);
					}					
				} catch (NumberFormatException e) {
					ModelUIPlugin.getPluginLog().logError("Incorrect priority value " + priorityString + ".");
				}
				Class editorClass = null;
				try {
					editorClass = bundle.loadClass(editor);
				} catch (ClassNotFoundException e) {
					if(ModelUIPlugin.getDefault().isDebugging()) {
						ModelUIPlugin.getPluginLog().logError("Cannot load editor class " + editor + " from " + es[i].getNamespaceIdentifier(),  e);
					}
					continue;
				}
				String contributor = cs[j].getAttribute("contributorClass");
				Class contributorClass = null;
				try {
					contributorClass = bundle.loadClass(contributor);
				} catch (ClassNotFoundException e) {
					if(ModelUIPlugin.getDefault().isDebugging()) {
						String message = "Cannot load contributor class " + contributor;
						ModelUIPlugin.getPluginLog().logError( message, e);
					}
					contributorClass = EditorActionBarContributor.class;
					////continue;
				}
				EditorPartFactory f = null;
				try {
					f = new EditorPartFactory(cs[j], editorClass, contributorClass);
				} catch (InstantiationException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				} catch (IllegalAccessException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
				StringTokenizer st = new StringTokenizer(entities, ",;");
				while(st.hasMoreTokens()) {
					String t = st.nextToken();
					Integer p = (Integer)priorities.get(t);
					int tp = (p == null) ? Integer.MAX_VALUE : p.intValue();
					if(priority < tp) {
						factories.put(t, f);
						priorities.put(t, Integer.valueOf(priority));
					}
				}
			}
		}
	}

	public EditorPartFactory getFactory(String entity) {
		return (EditorPartFactory)factories.get(entity); 
	}

}


