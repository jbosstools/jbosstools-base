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
package org.jboss.tools.common.model.ui.editors.dnd;
/**
 * 
 * @author eskimo
 *
 */
public class ValidationException extends Exception {
	private static final long serialVersionUID = 1L;

	boolean isWarning = false;
	/**
	 * 
	 * @param message
	 */
	public ValidationException(String message) {
		super(message);
	}
	
	public ValidationException(String message, boolean isWarning) {
		super(message);
		this.isWarning = isWarning;
	}

	public boolean isWarning() {
		return isWarning;
	}
	
}
