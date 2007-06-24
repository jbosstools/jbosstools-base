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
package org.jboss.tools.common.meta.key;

import java.net.URL;
import java.util.*;
import org.eclipse.core.runtime.*;
import org.osgi.framework.Bundle;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class KeyLoader {
	
	public static Properties load(String locale) {
		Properties keys = new Properties();
		Set set = getKeyResources(locale);
		
		Iterator it = set.iterator();
		if(!it.hasNext()) return keys;
		while(it.hasNext()) {
			URL url = (URL)it.next();
			try {
				Properties p = new Properties();
				p.load(url.openConnection().getInputStream());
				Enumeration ks = p.keys();
				while(ks.hasMoreElements()) {
					String k = (String)ks.nextElement();
					keys.setProperty(k, p.getProperty(k));
				}
			} catch (Exception e) {
				ModelPlugin.log("KeyLoader:load" + url);
			}
		}
		return keys;
	}

	private static Set getKeyResources(String locale) {
		Set<URL> resources = new HashSet<URL>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint("org.jboss.tools.common.model.keys");
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			Bundle bundle = Platform.getBundle(es[i].getNamespaceIdentifier());
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String path = elements[j].getAttribute("path");
				if(path.endsWith(".properties") && locale != null && locale.length() > 0) {
					path = path.substring(0, path.length() - ".properties".length()) + "_" + locale + ".properties";
				}
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
