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
package org.jboss.tools.common.kb.configuration;

/**
 * @author igels
 */
public interface KbConfiguration {
	
	public static final String SCHEMA_FOLDER_NAME = "schemas"; //$NON-NLS-1$
	public static final String SCHEMA_DTD_FOLDER_NAME = "dtd"; //$NON-NLS-1$
	public static final String SCHEMA_TLD_FOLDER_NAME = "tld"; //$NON-NLS-1$
	public static final String SCHEMA_HTML_FOLDER_NAME = "html"; //$NON-NLS-1$
	public static final String SCHEMA_JSP_FOLDER_NAME = "jsp"; //$NON-NLS-1$
	public static final String SCHEMA_HTML_FILE_NAME = "htmlschema.xml"; //$NON-NLS-1$
	public static final String SCHEMA_JSP_FILE_NAME = "jspschema.xml"; //$NON-NLS-1$
	public static final String SCHEMA_JSP_DIRECTIVE_FILE_NAME = "jspdirectiveschema.xml"; //$NON-NLS-1$
	public static final String HTML_MAP_FILE_NAME = "htmlmap.object";	 //$NON-NLS-1$

	/**
	 * 
	 * @return
	 */
	public String getDtdSchemaPath();

	/**
	 * 
	 * @return
	 */
	public String getTldCustomSchemaPath();

	/**
	 * 
	 * @return
	 */
	public String getHtmlSchemaFilePath();

	/**
	 * 
	 * @return
	 */
	public String getJspSchemaFilePath();

	/**
	 * 
	 * @return
	 */
	public String getJspDirectiveSchemaFilePath();

	/**
	 * 
	 * @return
	 */
	public String getHtmlMapFilePath();
}