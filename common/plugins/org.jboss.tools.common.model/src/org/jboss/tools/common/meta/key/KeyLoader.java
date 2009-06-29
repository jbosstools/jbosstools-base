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

import java.io.IOException;
import java.net.URL;
import java.util.*;
import org.eclipse.core.runtime.*;
import org.osgi.framework.Bundle;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class KeyLoader {
	
	public static Properties load(String locale) {
		Properties keys = new Properties();
		Set<URL> set = getKeyResources(locale);
		
		Iterator<URL> it = set.iterator();
		if(!it.hasNext()) return keys;
		while(it.hasNext()) {
			URL url = it.next();
			Properties p = new Properties();
			try {
				p.load(url.openConnection().getInputStream());
			} catch (IOException e) {
				ModelPlugin.getPluginLog().logError("KeyLoader:load" + url); //$NON-NLS-1$
			}
			Enumeration ks = p.keys();
			while(ks.hasMoreElements()) {
				String k = (String)ks.nextElement();
				keys.setProperty(k, p.getProperty(k));
			}
		}
		return keys;
	}

	private static Set<URL> getKeyResources(String locale) {
		Set<URL> resources = new HashSet<URL>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint("org.jboss.tools.common.model.keys"); //$NON-NLS-1$
		IExtension[] es = point.getExtensions();
		for (int i = 0; i < es.length; i++) {
			Bundle bundle = Platform.getBundle(es[i].getNamespaceIdentifier());
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				String path = elements[j].getAttribute("path"); //$NON-NLS-1$
				if(path.endsWith(".properties") && locale != null && locale.length() > 0) { //$NON-NLS-1$
					path = path.substring(0, path.length() - ".properties".length()) + "_" + locale + ".properties"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				try {
					URL url = bundle.getResource(path);
					if(url != null) {
						resources.add(url);
					}
				} catch (IllegalStateException e) {
					ModelPlugin.getPluginLog().logInfo("KeyLoader: Plugin " + es[i].getNamespaceIdentifier() + " is uninstalled"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
		return resources;
	}

}
