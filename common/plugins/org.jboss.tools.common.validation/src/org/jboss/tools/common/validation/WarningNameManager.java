/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

/**
 * Represents a map between warning name supported by @SuppressWarnings
 * and the corresponding preference ID.
 * 
 * @author Alexey Kazakov
 */
public class WarningNameManager implements IWarningNameMap {

	private static final WarningNameManager INSTANCE = new WarningNameManager();

	private Map<String, Set<IConfigurationElement>> allExtensions;
	private Map<String, Set<IWarningNameMap>> maps;
	private Map<String, String[]> warnings;

	public static WarningNameManager getInstance() {
		return INSTANCE;
	}

	private WarningNameManager() {
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IWarningNameMap#getNames(java.lang.String)
	 */
	@Override
	public String[] getWarningNames(String preferenceID) {
		init();
		String[] names = null;
		synchronized (warnings) {
			names = warnings.get(preferenceID);
			if(names==null) {
				String preferenceGoupID = getPreferenceGroupID(preferenceID);
				Set<IWarningNameMap> mapsByID = maps.get(preferenceGoupID);
				if(mapsByID==null) {
					mapsByID = new HashSet<IWarningNameMap>();
					maps.put(preferenceGoupID, mapsByID);
					Set<IConfigurationElement> extns = allExtensions.get(preferenceGoupID);
					if(extns!=null) {
						for (IConfigurationElement element : extns) {
							try {
								Object o = element.createExecutableExtension("class");
								if(o instanceof IWarningNameMap) {
									mapsByID.add((IWarningNameMap)o);
								} else {
									CommonValidationPlugin.getDefault().logError("Extension of " + IWarningNameMap.EXTENSION_POINT_ID + " should refer to an instance of " + IWarningNameMap.class + ". But the actuall class declared in " + element + " refers to " + o.getClass());
								}
							} catch (CoreException e) {
								CommonValidationPlugin.getDefault().logError(e);
							}
						}
					}
				}
				if(mapsByID.size()==1) {
					names = mapsByID.iterator().next().getWarningNames(preferenceID);
				} else {
					Set<String> mapNameSet = new HashSet<String>();
					for (IWarningNameMap map : mapsByID) {
						String[] mapNames = map.getWarningNames(preferenceID);
						for (String n : mapNames) {
							mapNameSet.add(n);
						}
					}
					names = mapNameSet.toArray(new String[mapNameSet.size()]);
				}
				warnings.put(preferenceID, names);
			}
		}
		return names;
	}

	private String getPreferenceGroupID(String preferenceID) {
		int dot = preferenceID.lastIndexOf('.');
		return dot>-1?preferenceID.substring(0, dot):preferenceID;
	}

	private synchronized void init() {
		if(allExtensions == null) {
			maps = new HashMap<String, Set<IWarningNameMap>>();
			warnings = new HashMap<String, String[]>();
			allExtensions = new HashMap<String, Set<IConfigurationElement>>();
	        IExtensionRegistry registry = Platform.getExtensionRegistry();
			IExtensionPoint extensionPoint = registry.getExtensionPoint(IWarningNameMap.EXTENSION_POINT_ID);
			if (extensionPoint != null) { 
				IExtension[] extensions = extensionPoint.getExtensions();
				for (int i=0; i<extensions.length; i++) {
					IExtension extension = extensions[i];
					IConfigurationElement[] elements = extension.getConfigurationElements();
					for(int j=0; j<elements.length; j++) {
						String preferenceGroupId = elements[j].getAttribute("preferenceGroupID");
						Set<IConfigurationElement> els = allExtensions.get(preferenceGroupId);
						if(els==null) {
							els = new HashSet<IConfigurationElement>();
							allExtensions.put(preferenceGroupId, els);
						}
						els.add(elements[j]);
					}
				}
			}
		}
	}
}