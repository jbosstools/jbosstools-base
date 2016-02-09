/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.foundation.ui.credentials.internal;

import org.eclipse.osgi.util.NLS;

public class CredentialMessages {
	
	private static String BUNDLE_NAME = CredentialMessages.class.getName();
	
	public static String AddDomain;
	public static String RemoveDomain;
	public static String AddUser;
	public static String RemoveUser;
	public static String Edit;
	public static String AddACredentialDomain;
	public static String EditACredentialDomain;
	public static String DomainNameExists;
	public static String DomainIdExists;
	public static String NewDomainNameLabel;
	public static String Warning;
	public static String UnableToDeleteCredentials;
	
	public static String DomainLabel;
	public static String AddACredentialLabel;
	public static String EditACredentialLabel;
	public static String UsernameLabel;
	public static String PasswordLabel;
	public static String AlwaysPromptForPasswordLabel;
	public static String SelectDomain;
	public static String UsernameCannotBeBlank;
	public static String PasswordCannotBeBlank;
	public static String UsernameAlreadyExists;
	
	

	public static String OK;
	public static String Cancel;
	public static String CredentialPrompterTitle;
	public static String DescriptionSectionTitle;
	public static String DescriptionSectionContent;
	public static String PasswordSectionTitle;
	public static String PasswordLabel2;
	public static String ShowPasswordLabel;
	public static String MessageEmptyPassword;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, CredentialMessages.class);
	}
}
