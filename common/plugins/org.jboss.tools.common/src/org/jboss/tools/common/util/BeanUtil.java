/*******************************************************************************
  * Copyright (c) 2010 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.util;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class BeanUtil {
	public static final String GET = "get"; //$NON-NLS-1$
	public static final String SET = "set"; //$NON-NLS-1$
	public static final String IS = "is"; //$NON-NLS-1$

	public static boolean isGetter(String methodName, int numberOfParameters) {
		return (((methodName.startsWith(GET) && !methodName.equals(GET))
					|| (methodName.startsWith(IS) && !methodName.equals(IS)))
				&& numberOfParameters == 0);
	}

	public static boolean isSetter(String methodName, int numberOfParameters) {
		return (((methodName.startsWith(SET) && !methodName.equals(SET)))
				&& numberOfParameters == 1);
	}

	public static String getPropertyName(String methodName) {
		if(isGetter(methodName, 0) || isSetter(methodName, 1)) {
			StringBuffer name = new StringBuffer(methodName);
			if(methodName.startsWith(IS)) {
				name.delete(0, 2);
			} else {
				name.delete(0, 3);
			}
			if(name.length() < 2 || Character.isLowerCase(name.charAt(1))) {
				name.setCharAt(0, Character.toLowerCase(name.charAt(0)));
			}
			return name.toString();
		}
		return null;
	}

}
