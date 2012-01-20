/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.templates.preferences;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.templates.preferences.messages"; //$NON-NLS-1$
	public static String CHECKBOX_LABEL;
	public static String ClassTemplateComponent_ADD;
	public static String ClassTemplateComponent_COLUMN_INTERFACES;
	public static String ClassTemplateComponent_EDIT;
	public static String ClassTemplateComponent_LABEL_BASECLASS;
	public static String ClassTemplateComponent_LABEL_INTERFACES;
	public static String ClassTemplateComponent_REMOVE;
	public static String ClassTemplateComponent_XPATH;
	public static String ClassTemplateComponent_LIST_OF_TEMPLATE_GROUPS;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
