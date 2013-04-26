/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.stacks.core;

import org.eclipse.osgi.util.NLS;

public class Messages {
	private static final String RESOURCE_NAME = "org.jboss.tools.stacks.core.messages"; //$NON-NLS-1$

	static {
		// initialize resource bundle
		NLS.initializeMessages(RESOURCE_NAME, Messages.class);
	}

	public Messages() {
	}

}
