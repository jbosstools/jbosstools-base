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

package org.jboss.tools.common.util;

/**
 * Miscellaneous String utility functions
 * 
 * @author Victor V. Rubezhny
 */
public class StringUtil {
/**
	 * Cuts of the starting and ending quotes from a given text value
	 * 
	 * @param Quoted text value
	 * @return Non-quoted text value 
	 */
	public static String trimQuotes(String value) {
		if(value == null)
			return null;

		if(value.startsWith("'") || value.startsWith("\"")) {  //$NON-NLS-1$ //$NON-NLS-2$
			value = value.substring(1);
		} 
		
		if(value.endsWith("'") || value.endsWith("\"")) { //$NON-NLS-1$ //$NON-NLS-2$
			value = value.substring(0, value.length() - 1);
		}
		return value;
	}

}
