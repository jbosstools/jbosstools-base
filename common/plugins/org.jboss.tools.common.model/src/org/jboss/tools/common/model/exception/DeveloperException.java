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
package org.jboss.tools.common.model.exception;

public class DeveloperException extends Exception {
	private static final long serialVersionUID = 1L;

	public DeveloperException(String message) {
		super(message);
	}

	public DeveloperException(String message, Throwable cause) {
		super(message, cause);
	}

	public DeveloperException(Throwable cause) {
		super(cause);
	}

}
