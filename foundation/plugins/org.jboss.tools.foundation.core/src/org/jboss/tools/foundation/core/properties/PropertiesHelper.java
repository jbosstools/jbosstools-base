/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties;

import org.jboss.tools.foundation.core.properties.internal.PropertiesProviderFactory;

/**
 * Utility class holding this installation's {@link IPropertiesProvider} instance.
 * 
 * @since 1.1.0
 */
public class PropertiesHelper {

	private static IPropertiesProvider propertiesProvider;

	private PropertiesHelper() {
	}
	
	/**
	 * Gets this installation's {@link IPropertiesProvider} instance.<br/>
	 * Developer Studio's {@link IPropertiesProvider}, if present, takes precedence over the default JBoss Tools one. 
	 */
	public synchronized static IPropertiesProvider getPropertiesProvider() {
		if (propertiesProvider == null) {
			propertiesProvider = new PropertiesProviderFactory().getPropertiesProvider();
		}
		return propertiesProvider;	
	}
}
