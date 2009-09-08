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
