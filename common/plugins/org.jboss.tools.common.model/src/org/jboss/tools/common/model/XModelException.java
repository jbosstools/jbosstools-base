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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.model.plugin.ModelPlugin;

/**
 * @author Viacheslav Kabanovich
 */
public class XModelException extends CoreException {
	private static final long serialVersionUID = 1L;
	
	public XModelException() {
		super(createStatus(null, null));
	}
	
	public XModelException(String message) {
		super(createStatus(message, null));
	}

	public XModelException(String message, Throwable cause) {
		super(createStatus(message, cause));
	}

	public XModelException(Throwable cause) {
		super(createStatus(null, cause));
	}
	
	static IStatus createStatus(String message, Throwable cause) {
		return new Status(IStatus.ERROR, ModelPlugin.PLUGIN_ID, message, cause);
		
	}
	
	public static void rethrow(Throwable cause) throws XModelException {
		if(cause instanceof XModelException) {
			throw (XModelException)cause;
		}
		throw new XModelException(cause);
	}

}
