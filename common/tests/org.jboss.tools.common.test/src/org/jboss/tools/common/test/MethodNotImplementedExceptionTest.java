/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.test;

import org.jboss.tools.common.Messages;
import org.jboss.tools.common.MethodNotImplementedException;

import junit.framework.TestCase;

public class MethodNotImplementedExceptionTest extends TestCase {

	public void testMethodNotImplementedException() {
		MethodNotImplementedException ex = new MethodNotImplementedException();
		assertEquals(
				Messages.MethodNotImplementedException_MethodIsNotImplementedYet,
				ex.getMessage());
	}

	public void testMethodNotImplementedExceptionStringThrowable() {
		Throwable th = new Exception();
		MethodNotImplementedException ex = new MethodNotImplementedException(Messages.BaseUIPlugin_ErrorDialogTitle,th);
		assertEquals(
				Messages.BaseUIPlugin_ErrorDialogTitle,
				ex.getMessage());
		assertEquals(th,ex.getCause());
	}

	public void testMethodNotImplementedExceptionString() {
		MethodNotImplementedException ex = new MethodNotImplementedException(Messages.BaseUIPlugin_ErrorDialogTitle);
		assertEquals(
				Messages.BaseUIPlugin_ErrorDialogTitle,
				ex.getMessage());
	}

	public void testMethodNotImplementedExceptionThrowable() {
		Throwable th = new Exception();
		MethodNotImplementedException ex = new MethodNotImplementedException(th);
		assertEquals(th,ex.getCause());
	}
}
