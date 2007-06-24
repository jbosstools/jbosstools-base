/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.jst.jsp.preferences.xpl;

/**
 * This class is a utility to generate contentType specific keys,
 * for content type sensitive preferences (eg. highlighting).
 * 
 * This class is not intended to be used for non content type
 * sensitive prefrences (eg. font style).
 * 
 * 
 */
public class PreferenceKeyGenerator {
	public static String generateKey(String key, String contentTypeId) {
		return contentTypeId + "." + key; //$NON-NLS-1$
	}
}