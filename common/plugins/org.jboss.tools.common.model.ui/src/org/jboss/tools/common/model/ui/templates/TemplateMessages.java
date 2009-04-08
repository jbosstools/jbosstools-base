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
package org.jboss.tools.common.model.ui.templates;

import org.eclipse.osgi.util.NLS;

public final class TemplateMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.templates.messages";//$NON-NLS-1$

	private TemplateMessages() {
		// Do not instantiate
	}

	public static String preferenceTextLabel;
	public static String propertyTextLabel;

	static {
		NLS.initializeMessages(BUNDLE_NAME, TemplateMessages.class);
	}
}