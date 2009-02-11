/**
 * 
 */
package org.jboss.tools.common.model.exception.test;

import org.jboss.tools.common.model.exception.DeveloperException;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class DeveloperExceptionTest extends TestCase {

	private final Throwable THROWABLE = new Throwable();
	private final String MESSAGE = "Message";
	
	/**
	 * @param name
	 */
	public DeveloperExceptionTest(String name) {
		super(name);
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.exception.DeveloperException#DeveloperException(java.lang.String)}.
	 */
	public void testDeveloperExceptionString() {
		DeveloperException ex = new DeveloperException(MESSAGE);
		assertEquals(MESSAGE, ex.getMessage());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.exception.DeveloperException#DeveloperException(java.lang.String, java.lang.Throwable)}.
	 */
	public void testDeveloperExceptionStringThrowable() {

		DeveloperException ex = new DeveloperException(MESSAGE, THROWABLE);
		assertEquals(MESSAGE, ex.getMessage());
		assertEquals(THROWABLE, ex.getCause());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.exception.DeveloperException#DeveloperException(java.lang.Throwable)}.
	 */
	public void testDeveloperExceptionThrowable() {
		DeveloperException ex = new DeveloperException(THROWABLE);
		assertEquals(THROWABLE, ex.getCause());
	}

}
