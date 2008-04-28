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
package org.jboss.tools.common.text.ext.hyperlink.xml;


/**
 * @author Jeremy
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class XMLRoleNameHyperlinkPartitioner extends XMLJumpToHyperlinkPartitioner {
	public static final String XML_ROLE_NAME_PARTITION = "org.jboss.tools.common.text.ext.xml.XML_ROLE_NAME";

	protected String getPartitionType() {
		return XML_ROLE_NAME_PARTITION;
	}

	protected String[] getValidAxisEndings() {
		return new String[] {"/servlet/security-role-ref/role-link/", 
				"/servlet/run-as/role-name/", 
				"/security-constraint/auth-constraint/role-name/"};
	}
}