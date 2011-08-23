/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.base.test.validation;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.validation.JBTValidationException;

/**
 * @author Alexey Kazakov
 */
public class ValidationExceptionLogger implements ILogListener {

	private Set<IStatus> exceptions = new HashSet<IStatus>();

	public ValidationExceptionLogger() {
		Platform.addLogListener(this);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.runtime.ILogListener#logging(org.eclipse.core.runtime.IStatus, java.lang.String)
	 */
	public void logging(IStatus status, String plugin) {
		exceptions.add(status);
	}

	public boolean hasExceptions() {
		return !getExceptions().isEmpty();
	}

	public Set<IStatus> getExceptions() {
		Set<IStatus> result = new HashSet<IStatus>();
		for (IStatus status : exceptions) {
			Throwable exception = status.getException();
			if(exception instanceof JBTValidationException) {
				result.add(status);
			}
		}
		return result;
	}
}