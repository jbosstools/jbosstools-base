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
package org.jboss.tools.common.model;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.messages"; //$NON-NLS-1$
	public static String ModelImages_NullImageName;
	public static String SpecialWizardSupport_BackArrow;
	public static String SpecialWizardSupport_Cancel;
	public static String SpecialWizardSupport_Close;
	public static String SpecialWizardSupport_Finish;
	public static String SpecialWizardSupport_Help;
	public static String SpecialWizardSupport_NextArrow;
	public static String SpecialWizardSupport_OK;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
