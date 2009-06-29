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
package org.jboss.tools.jst.web.tld;

import java.util.Properties;

import org.jboss.tools.common.model.XModelObject;

public interface IFilePathEncoder {
	public static String PATH_TYPE = "pathType"; //$NON-NLS-1$
	public static String PATH_ADDITION = "pathAddition"; //$NON-NLS-1$
	
	public static String ABSOLUTE_PATH = "absolute"; //$NON-NLS-1$
	public static String RELATIVE_PATH = "relative"; //$NON-NLS-1$
	
	/**
	 * Returns path that should be inserted to jsp page.
	 * 
	 * @path file path relative to WebContent
	 */
	public String encode(String path, XModelObject f, String tag, VpeTaglibManager taglibs, Properties context);
	
	/**
	 * Returns file path relative to WebContent
	 * 
	 * @path value on jsp page
	 */
	public String decode(String path, XModelObject f, String tag, VpeTaglibManager taglibs, Properties context);
}
