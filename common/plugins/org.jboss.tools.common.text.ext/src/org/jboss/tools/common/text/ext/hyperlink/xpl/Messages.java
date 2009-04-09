/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import org.eclipse.osgi.util.NLS;

public class Messages {

	
	private static final String BUNDLE_NAME = "org.jboss.tools.common.text.ext.hyperlink.xpl.Messages"; //$NON-NLS-1$
	private Messages() { }
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}
	
	public static String cannotOpenLink;

	public static String NotFound;
	public static String Open; 
	public static String OpenA; 
	public static String OpenAn; 
	public static String BrowseFor;
	public static String OpenFile;
	public static String OpenGetterOrSetterForProperty;
	public static String OpenValidationMethod;
	public static String OpenBean;
	public static String OpenClass;
	public static String OpenBundle;
	public static String OpenBundleProperty;
	public static String OpenCSSStyle;
	public static String OpenTagLibraryForPrefix;
	public static String OpenTagLibraryForUri;
	public static String TagLibrary;
	public static String File;
	public static String Getter;
	public static String Setter;
	public static String Bean;
	public static String Class;
	public static String Bundle;
	public static String BundleProperty;
	public static String CSSStyle;
	public static String ValidationMethod;
	public static String BrowseToFilterNameDefinition; 
	public static String BrowseToRoleNameDefinition; 
	public static String BrowseToServletNameDefinition; 
	public static String BrowseToTLDAttributeNameDeclaration;
	public static String BrowseForId;
	public static String Id;
	public static String BrowseForBeanId;
	public static String BeanId;

}
