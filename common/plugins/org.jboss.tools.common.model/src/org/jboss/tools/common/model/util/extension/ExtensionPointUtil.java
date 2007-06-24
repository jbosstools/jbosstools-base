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
package org.jboss.tools.common.model.util.extension;

import org.eclipse.core.runtime.*;

public class ExtensionPointUtil {

	public static Object findClassByElementId(String pointId, String id) throws Exception {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(pointId);
		return findClassByElementId(point, id);
	}

	public static Object findClassByElementId(IExtensionPoint point, String id) throws Exception {
		IConfigurationElement element = getElementById(point, id);
		if(element == null)
		  throw new NullPointerException("Configuration element with id=" + id + " is not found");
		String className = element.getAttribute("class");
		if(className == null || className.length() == 0)
		  throw new NullPointerException("Configuration element with id=" + id + " does not define 'class' attribute");
		return element.createExecutableExtension("class");
	}
	
	private static IConfigurationElement getElementById(IExtensionPoint point, String id) {
		IExtension[] extensions = point.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IConfigurationElement element = getElementById(extensions[i].getConfigurationElements(), id);
			if(element != null) return element;
		}
		return null;
	}

	private static IConfigurationElement getElementById(IConfigurationElement[] elements, String id) {
		for (int i = 0; i < elements.length; i++) 
			if(id.equals(elements[i].getAttribute("id"))) return elements[i];
		for (int i = 0; i < elements.length; i++) {
			IConfigurationElement element = getElementById(elements[i].getChildren(), id);
			if(element != null) return element;
		}
		return null;			
	}

}
