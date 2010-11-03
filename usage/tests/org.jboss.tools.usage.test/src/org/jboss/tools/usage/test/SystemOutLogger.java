/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import org.jboss.tools.common.log.ILoggingAdapter;

/**
 * @author Andre Dietisheim
 */
public class SystemOutLogger implements ILoggingAdapter {

	public void error(String errorMessage) {
		System.out.println("[ERROR]: " + errorMessage);
	}

	public void debug(String message) {
		System.out.println("[INFO]" + message);
	}
}
