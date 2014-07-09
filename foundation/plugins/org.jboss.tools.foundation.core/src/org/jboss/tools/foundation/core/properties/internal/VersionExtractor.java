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
package org.jboss.tools.foundation.core.properties.internal;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Utility class to extract the <code>version</code> property from a {@link ResourceBundle}
 * 
 * @author Fred Bricon
 * @since 1.1.0
 */
class VersionExtractor {

	private VersionExtractor() {
	}
	
	/**
	 * Searches for the <code>version</code> property in a given {@link ResourceBundle}. 
	 * <code>version</code> is a Maven-filtered property.
	 * As a convenience, this method searches for a <code>default.version</code> property if 
	 * Maven filtering is unavailable (for development purposes).   
	 * 
	 * @param bundleName the {@link ResourceBundle} name.
	 * @param classLoader the {@link ClassLoader} to look the <code>bundleName</code> at
	 * @return the <code>version</code> value or <code>null</code> if no match was found.
	 */
	public static String getVersion(String bundleName, ClassLoader classLoader) {
		ResourceBundle bundle = ResourceBundle.getBundle(bundleName, Locale.getDefault(), classLoader);
		String version = bundle.getString("version");
		if (version == null || version.trim().isEmpty() || version.contains("${")) {
			//Version not interpolated == not maven built, fallback on hard coded value,
			//for those m2e haters :-/
			version = bundle.getString("default.version");
		}
		return version.trim();
	}
	
}
