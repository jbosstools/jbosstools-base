/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.validation;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;

/**
 * @author Alexey Kazakov
 */
public class ValidationUtil {

	public final static Set<String> EXTNS;
	static {
		EXTNS = new HashSet<String>();
		EXTNS.add("java"); //$NON-NLS-1$
		EXTNS.add("xml"); //$NON-NLS-1$
	}

	/**
	 * Returns true if the file name has "java" or "xml" extension.
	 * @param file
	 * @return
	 */
	public static boolean checkFileExtensionForJavaAndXml(IFile file) {
		String ext = file.getFileExtension();
		if(ext!=null) {
			ext = ext.toLowerCase();
		}
		return EXTNS.contains(ext);
	}
}