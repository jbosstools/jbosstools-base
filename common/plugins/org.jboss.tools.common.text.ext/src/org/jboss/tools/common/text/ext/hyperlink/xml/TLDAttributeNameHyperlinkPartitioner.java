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
public class TLDAttributeNameHyperlinkPartitioner extends XMLJumpToHyperlinkPartitioner {
	public static final String TLD_ATTRIBUTE_NAME_PARTITION = "org.jboss.tools.common.text.ext.tld.TLD_ATTRIBUTE_NAME";

	protected String getPartitionType() {
		return TLD_ATTRIBUTE_NAME_PARTITION;
	}

	protected String[] getValidAxisEndings() {
		return new String[] {"/tag/variable/name-from-attribute/"};
	}
}
