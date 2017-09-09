/*******************************************************************************
 * Copyright (c) 2016-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.utils;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.reddeer.common.exception.RedDeerException;

/**
 * Class provisioning easy resource files lookup.
 * 
 * @author rhopp
 *
 */

public class ResourceLookup {

	/**
	 * Searches for resource file in provided plugin's "resources/" folder.
	 * 
	 * @param pluginId
	 *            ID of plugin. Obtainable by Activator.PLUGIN_ID.
	 * @param path
	 *            Path of desired resource file. Relative to plugin's
	 *            "resources" directory.
	 * @return
	 */

	public static File getResourceFile(String pluginId, String... path) {

		// Construct path
		StringBuilder builder = new StringBuilder();
		for (String fragment : path) {
			builder.append("/" + fragment);
		}

		String filePath = "";
		try {
			filePath = FileLocator.toFileURL(
					Platform.getBundle(pluginId).getEntry("/")).getFile()
					+ "resources" + builder.toString();
			File file = new File(filePath);
			if (!file.isFile()) {
				filePath = FileLocator.toFileURL(
						Platform.getBundle(pluginId).getEntry("/")).getFile()
						+ builder.toString();
			}
		} catch (IOException ex) {
			String message = filePath + " resource file not found";
			// log.error(message);
			throw new RedDeerException(message);
		}

		File file = new File(filePath);
		return file;
	}

}
