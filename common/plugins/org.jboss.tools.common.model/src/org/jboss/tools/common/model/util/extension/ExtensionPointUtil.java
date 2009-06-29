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

import java.text.MessageFormat;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.XModelException;

public class ExtensionPointUtil {

	public static Object findClassByElementId(String pointId, String id) throws CoreException {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(pointId);
		return findClassByElementId(point, id);
	}

	public static Object findClassByElementId(IExtensionPoint point, String id) throws CoreException {
		IConfigurationElement element = getElementById(point, id);
		if(element == null)
		  throw new XModelException(MessageFormat.format("Configuration element with id={0} is not found",
				id));
		String className = element.getAttribute("class"); //$NON-NLS-1$
		if(className == null || className.length() == 0)
		  throw new XModelException(MessageFormat
				.format(
						"Configuration element with id={0} does not define ''class'' attribute",
						id));
		return element.createExecutableExtension("class"); //$NON-NLS-1$
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
			if(id.equals(elements[i].getAttribute("id"))) return elements[i]; //$NON-NLS-1$
		for (int i = 0; i < elements.length; i++) {
			IConfigurationElement element = getElementById(elements[i].getChildren(), id);
			if(element != null) return element;
		}
		return null;			
	}

}
