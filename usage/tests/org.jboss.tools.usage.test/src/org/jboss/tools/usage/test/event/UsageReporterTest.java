/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.event;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.googleanalytics.RequestType;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.test.fakes.TestEventRegister;
import org.jboss.tools.usage.test.fakes.TestUsageReporter;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class UsageReporterTest {

	public static final long DAY = 1000*60*60*24;
	public static final long TODAY = System.currentTimeMillis();
	public static final long YESTERDAY = TODAY - DAY;

	@Test
	public void testTrackEvent() {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();

		register.setCurrentTime(YESTERDAY);
		UsageEventType type = new UsageEventType("usage", "1.2.100", "central", "showOnStartup", "true/false");
		boolean ok = reporter.trackEventSynchronously("/tools/usage/action/wsstartup/1.2.100", "Usage Test", type.event("true"), RequestType.PAGE, true);
		assertFalse(ok);

		assertTrackEvent(reporter, register);

		register.setCurrentTime(TODAY);
		ok = reporter.trackEventSynchronously("/tools/usage/action/wsstartup/1.2.100", "Usage Test", type.event("true"), RequestType.PAGE, true);
		assertTrue(ok);
		assertTrackEvent(reporter, register);
	}

	@Test
	public void testCountEvent() {
		assertCountEvent(false);
	}

	@Test
	public void testCountEventResetPreferences() {
		assertCountEvent(true);
	}

	@Test
	public void testRegisterCountEvent() {
		assertRegisterCountEvent(false);
	}

	@Test
	public void testRegisterCountEventResetPreferences() {
		assertRegisterCountEvent(true);
	}

	@Test
	public void testCheckCountEventPreferences() {
		assertCheckCountEvent(false);
	}

	@Test
	public void testCheckCountEventResetPreferences() {
		assertCheckCountEvent(true);
	}

	@Test
	public void testTrackCountEvents() {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();
		register.reset(true);

		register.setCurrentTime(YESTERDAY);
		UsageEventType type = new UsageEventType("test", "1.0.0", null, "test-count-events", "Label", "how many a day");
		int number = reporter.registerEventSynchronously(type);
		assertEquals(0, number);

		int result = reporter.trackCountEventsSynchronously();
		assertEquals(0, result);

		boolean ok = reporter.countEventSynchronously(type.event("test-label", 1));
		assertFalse(ok);

		result = reporter.trackCountEventsSynchronously();
		assertEquals(0, result);

		register.setCurrentTime(TODAY);
		result = reporter.trackCountEventsSynchronously();
		assertEquals(1, result);
		result = reporter.trackCountEventsSynchronously();
		assertEquals(0, result);
	}

	private void assertTrackEvent(TestUsageReporter reporter, TestEventRegister register) {
		UsageEventType type = new UsageEventType("usage", "1.2.100", "central", "showOnStartup", "true/false");

		int number = reporter.registerEventSynchronously(type);
		assertEquals(0, number);
		boolean ok = reporter.trackEventSynchronously("/tools/usage/action/wsstartup/1.2.100", "Usage Test", type.event("true"), RequestType.PAGE, true);
		assertTrue(ok);

		type = new UsageEventType("test", "1.0.0", null, "test", "Label", "1 - success, 0 - failure");
		number = reporter.registerEventSynchronously(type);
		assertEquals(0, number);
		ok = reporter.trackEventSynchronously(type.event("test-label", 1));
		assertTrue(ok);

		type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test-action");
		number = reporter.registerEventSynchronously(type);
		assertEquals(0, number);
		ok = reporter.trackEventSynchronously(type.event());
		assertTrue(ok);
	}

	private void assertCheckCountEvent(boolean reset) {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();
		boolean oldReset = register.setReset(reset);
		String action = reset?"count-test5":"count-test6";

		try {
			register.setCurrentTime(YESTERDAY);
			UsageEventType type = new UsageEventType("test", "1.0.0", null, action, "label", "1 - success, 0 - failure");
			int number = reporter.registerEventSynchronously(type);
			assertEquals(0, number);
			for (int i = 0; i < 3; i++) {
				UsageEvent event = type.event("test-label-" + i, 1);
				boolean ok = reporter.countEventSynchronously(event);
				assertFalse(ok);
				ok = reporter.countEventSynchronously(event);
				assertFalse(ok);
			}
			UsageEventType type2 = new UsageEventType("test", "1.0.0", null, "count-test7", "label", "1 - success, 0 - failure");
			number = reporter.registerEventSynchronously(type2);
			assertEquals(0, number);

			register.setCurrentTime(TODAY);
			number = reporter.checkCountEventSynchronously(type);
			assertEquals(3, number);
			number = reporter.checkCountEventSynchronously(type2);
			assertEquals(0, number);
		} finally {
			register.setReset(oldReset);
		}
	}

	private void assertCountEvent(boolean reset) {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();
		boolean oldReset = register.setReset(reset);
		String action = reset?"count-test":"count-test2";

		try {
			register.setCurrentTime(YESTERDAY);
			UsageEventType type = new UsageEventType("test", "1.0.0", null, action, "label", "1 - success, 0 - failure");
			int number = reporter.registerEventSynchronously(type);
			assertEquals(0, number);
			for (int i = 0; i < 3; i++) {
				boolean ok = reporter.countEventSynchronously(type.event("test-label", 1));
				assertFalse(ok);
			}

			register.setCurrentTime(TODAY);
			boolean ok = reporter.countEventSynchronously(type.event("test-label", 1));
			assertTrue(ok);

			ok = reporter.countEventSynchronously(type.event("test-label", 1));
			assertFalse(ok);
		} finally {
			register.setReset(oldReset);
		}
	}

	private void assertRegisterCountEvent(boolean reset) {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();
		boolean oldReset = register.setReset(reset);
		String action = reset?"count-test3":"count-test4";

		try {
			register.setCurrentTime(YESTERDAY);
			UsageEventType type = new UsageEventType("test", "1.0.0", null, action, "label", "1 - success, 0 - failure");
			int number = reporter.registerEventSynchronously(type);
			assertEquals(0, number);
			for (int i = 0; i < 3; i++) {
				boolean ok = reporter.countEventSynchronously(type.event("test-label", 1));
				assertFalse(ok);
			}

			register.setCurrentTime(TODAY);
			number = reporter.registerEventSynchronously(type);
			assertEquals(1, number);
			boolean ok = reporter.countEventSynchronously(type.event("test-label", 1));
			assertFalse(ok);

			ok = reporter.countEventSynchronously(type.event("test-label", 1));
			assertFalse(ok);
		} finally {
			register.setReset(oldReset);
		}
	}
}