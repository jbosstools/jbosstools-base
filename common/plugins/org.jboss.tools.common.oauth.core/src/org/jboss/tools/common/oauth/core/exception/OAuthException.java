/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.oauth.core.exception;

public abstract class OAuthException extends RuntimeException {

	private static final long serialVersionUID = -66495414279364584L;

	public OAuthException() {
	}

	public OAuthException(String message) {
		super(message);
	}

	public OAuthException(Throwable cause) {
		super(cause);
	}

	public OAuthException(String message, Throwable cause) {
		super(message, cause);
	}

	public OAuthException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
