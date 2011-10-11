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
 * Converts the severity of an IStatus to a boolean on behalf of a severity
 * mask.
 * 
 * @author André Dietisheim
 * 
 * @see IStatus
 * @see IStatus#getSeverity()
 * 
 */
public class StatusSeverity2BooleanConverter extends Converter {

	private int severity;

	/**
	 * Instantiates a new converter that turns the severity of a status into a
	 * boolean. The conversion is operated according to a severity mask that is
	 * given at construction time. If the status that is handed over at runtime
	 * matches the severity mask (given at construction time), then the
	 * converter returns a <code>true</code>. It will return <code>false</code>
	 * otherwise.
	 * 
	 * @param trueSeverityMask
	 *            the severity mask that is considered as equivalent to
	 *            <code>true</code>
	 */
	public StatusSeverity2BooleanConverter(int trueSeverityMask) {
		super(IStatus.class, Boolean.class);
		this.severity = trueSeverityMask;
	}

	/**
	 * Compares the severity of the given IStatus to the severity mask that was
	 * given at construction time. Returns <code>true</code> if the severity
	 * matches the mask, <code>false</code> otherwise.
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
		if (current == IStatus.OK) {
			return severity == IStatus.OK;
		}
		return (severity | current) == severity;
	}
}