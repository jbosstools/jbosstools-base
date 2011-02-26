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
package org.jboss.tools.common.text.xml;

import org.eclipse.osgi.util.NLS;

public class TextXMLMessages extends NLS{
	private static final String BUNDLE_NAME = "org.jboss.tools.common.text.xml.TextXMLMessages"; //$NON-NLS-1$
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, TextXMLMessages.class);
	}
	
	public static String SINGLE_QUICK_FIX;
	public static String MULTIPLE_QUICK_FIX;
}
