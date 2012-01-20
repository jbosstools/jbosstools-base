/*******************************************************************************
  * Copyright (c) 2009 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.gef;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.gef.messages"; //$NON-NLS-1$
	public static String DefaultPaletteCustomizer_ErrorMessage;
	public static String DiagramCopyAction_AcceleratorText;
	public static String DiagramCutAction_AcceleratorText;
	public static String DiagramPasteAction_AcceleratorText;
	public static String PrintRetargetAction_Name;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
