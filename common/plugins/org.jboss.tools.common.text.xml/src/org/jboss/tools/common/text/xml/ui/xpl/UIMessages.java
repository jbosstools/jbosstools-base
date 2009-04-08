/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
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
package org.jboss.tools.common.text.xml.ui.xpl;

import org.eclipse.osgi.util.NLS;

public final class UIMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.text.xml.ui.xpl.UIMessages";//$NON-NLS-1$

	private UIMessages() {
		// Do not instantiate
	}

	public static String SelectionListenerWithSMManager_job_title;
	public static String RedHatStructuredTextEditor_markOccurrences_job_name;

	static {
		NLS.initializeMessages(BUNDLE_NAME, UIMessages.class);
	}
}