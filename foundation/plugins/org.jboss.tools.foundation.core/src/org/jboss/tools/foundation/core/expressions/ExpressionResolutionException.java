/*************************************************************************************
 * Copyright (c) 2008-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.foundation.core.expressions;

/**
 * Thrown to indicate that an attempt has been made to resolve
 * expression ${sysPropertyName:defaultValue} in URL and it has
 * failed to parse expression or to resolve it to actual value
 * @author eskimo
 * @since 1.1
 */
public class ExpressionResolutionException extends RuntimeException {

	private static final long serialVersionUID = -4932531618750451234L;

	public ExpressionResolutionException(String message) {
		super(message);
	}
	public ExpressionResolutionException(Throwable parent) {
		super(parent);
	}
}
