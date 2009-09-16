/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common;

/**
 * @author eskimo
 *
 */
public class MethodNotImplementedException extends RuntimeException {

	public MethodNotImplementedException() {
		super(Messages.MethodNotImplementedException_MethodIsNotImplementedYet); //$NON-NLS-1$
	}

	public MethodNotImplementedException(String message, Throwable cause) {
		super(message, cause);
	}

	public MethodNotImplementedException(String message) {
		super(message);
	}

	public MethodNotImplementedException(Throwable cause) {
		super(cause);
	}

}
