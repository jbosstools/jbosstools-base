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
package org.jboss.tools.common.model;

/**
 * @author Viacheslav Kabanovich
 */
public class XModelException extends Exception {
	private static final long serialVersionUID = 1L;
	
	public XModelException() {}
	
	public XModelException(String message) {
		super(message);
	}

	public XModelException(String message, Throwable cause) {
		super(message, cause);
	}

	public XModelException(Throwable cause) {
		super(cause);
	}

}
