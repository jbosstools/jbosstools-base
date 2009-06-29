/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.filesystems;

import java.io.File;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.osgi.framework.Bundle;

/**
 * @author Viacheslav Kabanovich
 */
public class FilePathHelper {
	private static Check check = null;
	
	public static boolean isCaseSensitive() {
		if(check == null) check = new Check();
		return check.isCaseSensitive;
	}
	
	public static String toPathPath(String name) {
		return name == null || isCaseSensitive() ? name : name.toLowerCase();
	}
	
	private static class Check {
		boolean isCaseSensitive = false;
		public Check() {
			Bundle b = Platform.getBundle(ModelPlugin.PLUGIN_ID);
			String path = EclipseResourceUtil.getInstallPath(b);
			if(path == null) return;
			String file = path + "/images/default.gif"; //$NON-NLS-1$
			if(!new File(file).isFile()) {
				ModelPlugin.getPluginLog().logWarning("Cannot find file " + file); //$NON-NLS-1$
				return;
			}
			String file2 = path + "/images/Default.gif"; //$NON-NLS-1$
			if(!new File(file2).isFile()) {
				isCaseSensitive = true;
			}
		}
	}

}
