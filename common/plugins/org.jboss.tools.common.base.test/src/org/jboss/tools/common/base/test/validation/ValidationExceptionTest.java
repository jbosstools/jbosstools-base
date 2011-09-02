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

import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.common.validation.CommonValidationPlugin;
import org.jboss.tools.common.validation.JBTValidationException;

/**
 * @author Alexey Kazakov
 */
public class ValidationExceptionTest extends TestCase {

	protected volatile static ValidationExceptionLogger LOGGER;

	public static ValidationExceptionLogger initLogger() {
		LOGGER = new ValidationExceptionLogger();
		return LOGGER;
	}

	public static void assertExceptionsIsEmpty(ValidationExceptionLogger logger) throws Exception {
		Set<IStatus> exceptions = logger.getExceptions();
		StringBuffer error = new StringBuffer("The following exceptions were thrown during project validation:");
		for (IStatus status : exceptions) {
			Throwable cause = status.getException().getCause();
			error.append("\r\n").append(status.toString()).append(":");
			if(cause!=null) {
				error.append(cause.toString()).append(":");
				if(cause.getStackTrace()!=null && cause.getStackTrace().length>0) {
					error.append(cause.getStackTrace()[0].toString());
				}
			}
		}
		assertTrue(error.toString(), exceptions.isEmpty());
	}

	public void testExceptions() throws Exception {
		assertExceptionsIsEmpty(LOGGER);
	}

	public void testLogging() {
		ValidationExceptionLogger logger = new ValidationExceptionLogger();
		CommonValidationPlugin.getDefault().logError(new JBTValidationException("Test logging", null));
		Set<IStatus> exceptions = logger.getExceptions();
		assertEquals(1, exceptions.size());
	}
}