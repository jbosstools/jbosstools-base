/**
 * 
 */
package org.jboss.tools.common.model.test;

import org.jboss.tools.common.model.XModelTransferBuffer;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class XModelTransferBufferTest extends TestCase {

	/**
	 * Test method for {@link org.jboss.tools.common.model.XModelTransferBuffer#getInstance()}.
	 */
	public void testGetInstance() {
		assertNotNull(XModelTransferBuffer.getInstance());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.XModelTransferBuffer#enable()}.
	 */
	public void testEnable() {
		XModelTransferBuffer.getInstance().enable();
		assertTrue(XModelTransferBuffer.getInstance().isEnabled());
		assertNotNull(XModelTransferBuffer.getInstance().getBuffer());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.XModelTransferBuffer#disable()}.
	 */
	public void testDisable() {
		XModelTransferBuffer.getInstance().disable();
		assertFalse(XModelTransferBuffer.getInstance().isEnabled());
		assertNull(XModelTransferBuffer.getInstance().getBuffer());
	}

}
