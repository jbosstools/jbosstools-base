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
package org.jboss.tools.common.verification.vrules.model;

import java.net.URL;
import java.util.*;
import org.eclipse.core.runtime.*;
import org.osgi.framework.Bundle;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class RuleSetResourceLoader {
	
	public static Set getResources(String extensionPointId) {
		Set<URL> resources = new HashSet<URL>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(extensionPointId);
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			Bundle bundle = Platform.getBundle(es[i].getNamespaceIdentifier());
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String path = elements[j].getAttribute("path");
				if(path == null) continue;
				try {
					URL url = bundle.getResource(path);
					if(url != null) {
						resources.add(url);
					} else {
						if(ModelPlugin.isDebugEnabled()) {
							ModelPlugin.log("Warning: meta resource " + path + " not found.");
						}
					}
				} catch (Exception e) {
					if(ModelPlugin.isDebugEnabled()) {
						ModelPlugin.log("Warning: meta resource " + path + " not found.");
					}
				}
			}
		}
		return resources;
	}

}
