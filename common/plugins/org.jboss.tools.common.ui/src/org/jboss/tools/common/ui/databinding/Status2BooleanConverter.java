/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.conversion.Converter;
import org.eclipse.core.runtime.IStatus;

/**
 * Converts an IStatus to a boolean.
 * 
 * @author André Dietisheim
 * 
 * @see IStatus
 * 
 */
public class Status2BooleanConverter extends Converter {

	private int severity;

	public Status2BooleanConverter(int severity) {
		super(IStatus.class, Boolean.class);
		this.severity = severity;
	}

	/**
	 * Compares the given IStatus to the status that was given at construction
	 * time. Returns <code>true</code> if they're <b>not</b> equal, <code>false</code>
	 * otherwise.
	 * 
	 * @see IStatus#ERROR
	 * @see IStatus#WARNING
	 * @see IStatus#INFO
	 * @see IStatus#OK
	 */
	public Object convert(Object fromObject) {
		if (!(fromObject instanceof IStatus)) {
			return Boolean.FALSE;
		}

		int current = ((IStatus) fromObject).getSeverity();
//		switch (severity) {
//		default:
//		case IStatus.ERROR:
//			return current != IStatus.ERROR;
//		case IStatus.WARNING:
//			return current != IStatus.WARNING;
//		case IStatus.INFO:
//			return current != IStatus.INFO;
//		case IStatus.OK:
//			return current != IStatus.OK;
//		}
		if (current == IStatus.OK) {
			System.err.println("converter result = " + (severity == IStatus.OK) + "<- current = " + current + " configured = " + severity);
			return severity == IStatus.OK;
		}
		System.err.println("converter result = " +  ((severity | current) == severity));
		return (severity | current) == severity;
	}
}