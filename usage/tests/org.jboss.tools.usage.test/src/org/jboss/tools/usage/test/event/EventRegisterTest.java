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

import java.util.HashMap;
import java.util.Map;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.event.UsageReporter;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.event.EventRegister;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;
import org.jboss.tools.usage.test.fakes.GlobalUsageSettingsForEventRegisterTest;
import org.jboss.tools.usage.test.fakes.TestEventRegister;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class EventRegisterTest {

	@Test
	public void testUsageDisabled() {
		EventRegister register = getRegister();
		UsageEventType type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test-action");
		UsageEvent event = type.event();

		Map<Object, Object> map = new HashMap<Object, Object>();
		map.put("usage_reporting_enabled", "false");

		GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), map); 
		register.registerEvent(type);
		assertFalse(register.checkTrackData(event, settings, false).isOkToSend());

		settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), new HashMap<Object, Object>()); 
		assertFalse(register.checkTrackData(event, settings, false).isOkToSend());

		settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap()); 
		assertTrue(register.checkTrackData(event, settings, false).isOkToSend());
	}

	@Test
	public void testRegistration() {
		EventRegister register = getRegister();
		UsageEventType type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test-action", "test-label-description", "test-value-description");
		UsageEvent event = type.event("test-label", 1);

		GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap()); 
		assertFalse(register.checkTrackData(event, settings, false).isOkToSend());

		register.registerEvent(type);
		assertTrue(register.checkTrackData(event, settings, false).isOkToSend());
	}

	@Test
	public void testUnlimitedEvent() {
		UsageEventType type = new UsageEventType("test2", "1.0.0", null, "remove", "test-label-description", "test-value-description");
		assertEventEnabled(type);

		type = new UsageEventType("test2", "1.0.0", null, "remove");
		assertEventEnabled(type);

		type = new UsageEventType("test", "1.0.0", null, "new", "AS7-1", "test-value-description");
		assertEventEnabled(type);

		type = new UsageEventType("test3", "1.0.0", null, "new");
		assertEventEnabled(type);
	}

	@Test
	public void testDisabledEvent() {
		UsageEventType type = new UsageEventType("test2", "1.0.0", null, "add", "test-label-description", "test-value-description");
		assertEventDisabled(type);

		type = new UsageEventType("test2", "1.0.0", null, "add", "test-label-description");
		assertEventDisabled(type);

		type = new UsageEventType("test2", "1.0.0", null, "new");
		assertEventDisabled(type);
	}

	@Test
	public void testLimitedEvent() {
		assertLimitedEvent(EventRegister.getInstance());
	}

	@Test
	public void testLimitedEventWithTestRegister() {
		TestEventRegister register = TestEventRegister.getInstance();
		register.setCurrentTime(YESTERDAY);
		assertLimitedEvent(register);
	}

	private static final long DAY = 1000*60*60*24;
	private static final long TODAY = System.currentTimeMillis();
	private static final long YESTERDAY = TODAY - DAY;
	private static final long DAY_BEFORE_YESTERDAY = YESTERDAY - DAY;
	private static final long THREE_DAYS_BEFORE = DAY_BEFORE_YESTERDAY - DAY;

	@Test
	public void testDailyEvent() {
		TestEventRegister register = TestEventRegister.getInstance();
		register.setCurrentTime(THREE_DAYS_BEFORE);
		assertLimitedEvent(register);
		register.setCurrentTime(DAY_BEFORE_YESTERDAY);
		assertLimitedEvent(register);

		boolean old = register.setReset(false);
		try {
			register.setCurrentTime(YESTERDAY);
			assertLimitedEvent(register);
			register.setCurrentTime(TODAY);
			assertLimitedEvent(register);
		} finally {
			register.setReset(old);
		}
	}

	private static final String LONG_NAME_SUFIX = "long-name-long-name-long-name-long-name-long-name";

	@Test
	public void testCountEventNoResetPreferences() {
		assertCountEvent(false, "");
	}

	@Test
	public void testCountEventNoResetPreferencesLongName() {
		assertCountEvent(false, LONG_NAME_SUFIX);
	}

	@Test
	public void testCountEvent() {
		assertCountEvent(true, "");
	}

	@Test
	public void testCountEventLongName() {
		assertCountEvent(true, LONG_NAME_SUFIX);
	}

	@Test
	public void testCountEventWithBreakNoResetPreferences() {
		assertCountEventWithBreak(false, "");
	}

	@Test
	public void testCountEventWithBreakNoResetPreferencesLongName() {
		assertCountEventWithBreak(false, LONG_NAME_SUFIX);
	}

	@Test
	public void testCountEventWithBreak() {
		assertCountEventWithBreak(true, "");
	}

	@Test
	public void testCountEventWithBreakLongName() {
		assertCountEventWithBreak(true, LONG_NAME_SUFIX);
	}

	public void assertCountEvent(boolean reset, String nameSufix) {
		TestEventRegister register = TestEventRegister.getInstance();

		boolean old = register.setReset(reset);
		try {
			register.setCurrentTime(THREE_DAYS_BEFORE);
			GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap());
			UsageEventType type = new UsageEventType("test" + nameSufix, "1.0.0", null, "wizard" + reset + nameSufix, "test-label-description", "test-value-description");
			register.registerEvent(type);
			UsageEvent event = new UsageEvent(type, "test-label", 1);
			for (int i = 0; i < 3; i++) {
				EventRegister.Result result = register.checkTrackData(event, settings, true);
				assertFalse(result.isOkToSend());
			}

			event = new UsageEvent(type, "test-label", 2);
			EventRegister.Result result = register.checkTrackData(event, settings, true);
			assertFalse(result.isOkToSend());

			register.setCurrentTime(DAY_BEFORE_YESTERDAY);
			event = new UsageEvent(type, "test-label", 2);
			result = register.checkTrackData(event, settings, true);
			assertTrue(result.isOkToSend());
			assertEquals(5, result.getPreviousSumOfValues());
			assertEquals("test-label", result.getCountEventLabel());

			register.setCurrentTime(YESTERDAY);
			event = new UsageEvent(type, "test-label3", 100);
			result = register.checkTrackData(event, settings, true);
			assertTrue(result.isOkToSend());
			assertEquals(2, result.getPreviousSumOfValues());
			assertEquals(UsageReporter.NOT_APPLICABLE_LABEL, result.getCountEventLabel());

			result = register.checkTrackData(event, settings, true);
			assertFalse(result.isOkToSend());

			register.setCurrentTime(TODAY);
			event = new UsageEvent(type, "test-label3", 0);
			result = register.checkTrackData(event, settings, true);
			assertTrue(result.isOkToSend());
			assertEquals(200, result.getPreviousSumOfValues());
			assertEquals(UsageReporter.NOT_APPLICABLE_LABEL, result.getCountEventLabel());
		} finally {
			register.setReset(old);
		}
	}

	public void assertCountEventWithBreak(boolean reset, String nameSufix) {
		TestEventRegister register = TestEventRegister.getInstance();
		boolean old = register.setReset(reset);
		try {
			register.setCurrentTime(THREE_DAYS_BEFORE);
			GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap());
			UsageEventType type = new UsageEventType("test" + nameSufix, "1.0.0", null, "wizard1" + reset + nameSufix, "test-label-description", "test-value-description");
			register.registerEvent(type);
			UsageEvent event = new UsageEvent(type, "test-label", 1);
			EventRegister.Result result = register.checkTrackData(event, settings, true);
			assertFalse(result.isOkToSend());

			event = new UsageEvent(type, "test-label2", 2);
			result = register.checkTrackData(event, settings, true);
			assertFalse(result.isOkToSend());

			register.setCurrentTime(TODAY);
			event = new UsageEvent(type, "test-label3", 0);
			result = register.checkTrackData(event, settings, true);
			assertTrue(result.isOkToSend());
			assertEquals(3, result.getPreviousSumOfValues());
		} finally {
			register.setReset(old);
		}
	}

	private EventRegister getRegister() {
		return EventRegister.getInstance();
//		return TestEventRegister.getInstance();
	}

	private Map<Object, Object> getMap() {
		Map<Object, Object> map = new HashMap<Object, Object>();
		/*
		 *   usage_reporting_enabled=true
		 *   test2=0
		 *   test2.remove=-1
		 *   test.new=3
		 *   test.new.AS7-1=-1
		 *   test.new.AS7.2=4
		 *   test.run=5
		*/
		map.put("usage_reporting_enabled", "true");
		map.put("test2", "0");
		map.put("test2.remove", "-1");
		map.put("test.new", "3");
		map.put("test.new.AS7-1", "-1");
		map.put("test.new.AS7.2", "-1");
		map.put("test.new.AS7.3", "0");
		map.put("test.run", "5");
		return map;
	}

	public void assertLimitedEvent(EventRegister register) {
		UsageEventType type = new UsageEventType("test", "1.0.0", null, "new", "test-label-description", "test-value-description");
		UsageEvent event = new UsageEvent(type, "test-label", 3);
		GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap());
		assertLimited(register, event, settings, 3);

		type = new UsageEventType("test", "1.0.0", null, "new", "AS7-1", "test-value-description");
		assertEvent(register, true, type);

		type = new UsageEventType("test", "1.0.0", null, "new", "AS7.2", "test-value-description");
		assertEvent(register, true, type);

		type = new UsageEventType("test", "1.0.0", null, "new", "AS7.3", "test-value-description");
		assertEvent(register, false, type);

		type = new UsageEventType("test", "1.0.0", null, "new", "AS7.4", "test-value-description");
		assertEvent(register, false, type);

		type = new UsageEventType("test", "1.0.0", null, "run");
		event = new UsageEvent(type);
		assertLimited(register, event, settings, 5);
	}

	private void assertLimited(EventRegister register, UsageEvent event, GlobalUsageSettings settings, int limit) {
		register.registerEvent(event.getType());
		for (int i = 0; i < limit; i++) {
			assertTrue("Reproting is disabled for the attempt number " + (i + 1) + " but the limit is " + limit + " reports a day.", register.checkTrackData(event, settings, false).isOkToSend());
		}
		for (int i = 0; i < limit; i++) {
			assertFalse("Reproting is enabled for the attempt number " + (limit + i + 1) + " but the limit is " + limit + " reports a day.", register.checkTrackData(event, settings, false).isOkToSend());
		}
	}

	private void assertEventEnabled(UsageEventType type) {
		assertEvent(getRegister(), true, type);
	}

	private void assertEventDisabled(UsageEventType type) {
		assertEvent(getRegister(), false, type);
	}

	private void assertEvent(EventRegister register, boolean enabled, UsageEventType type) {
		register.registerEvent(type);
		UsageEvent event;
		if(type.getLabelDescription()==null && type.getValueDescription()==null) {
			event = new UsageEvent(type);
		} else if(type.getValueDescription()==null) {
			event = new UsageEvent(type, type.getLabelDescription());
		} else {
			event = new UsageEvent(type, type.getLabelDescription(), 1);
		}
		GlobalUsageSettings settings = new GlobalUsageSettingsForEventRegisterTest(JBossToolsUsageActivator.getDefault(), getMap());
		assertEquals(enabled, register.checkTrackData(event, settings, false).isOkToSend());
	}
}