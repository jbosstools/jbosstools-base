package org.jboss.tools.common.test;

import org.jboss.tools.common.Messages;

import junit.framework.TestCase;

public class MessagesTest extends TestCase {
	public static void testMessagesInitialization() {
		assertNotNull(Messages.BaseUIPlugin_ErrorDialogTitle);
	}
}
