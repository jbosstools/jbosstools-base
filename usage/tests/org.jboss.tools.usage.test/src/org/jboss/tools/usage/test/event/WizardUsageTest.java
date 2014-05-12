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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;

import org.eclipse.jface.wizard.WizardDialog;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.internal.reporting.UsageReportDispatcher;
import org.jboss.tools.usage.internal.reporting.WizardListener;
import org.jboss.tools.usage.test.fakes.TestEventRegister;
import org.jboss.tools.usage.test.fakes.TestUsageReporter;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class WizardUsageTest {

	@Test
	public void testCountEvent() {
		TestUsageReporter reporter = TestUsageReporter.getInstance();

		UsageEventType type = UsageReportDispatcher.createFinishWizardType();
		boolean ok = reporter.registerEventSynchronously(type);
		assertFalse(ok);

		assertWizardEvent(type, "org.jboss.tools.usage.test.SomeWizardClassName1", 5);

		assertWizardEvent(type, "org.jboss.tools.usage.test.SomeWizardClassName2", 7);
	}

	@Test
	public void testWizardAPI() {
		try {
			Field field = WizardDialog.class.getDeclaredField(WizardListener.FINISH_BUTTON_FIELD_NAME);
			field.setAccessible(true);
			field = WizardDialog.class.getDeclaredField(WizardListener.CANCEL_BUTTON_FIELD_NAME);
			field.setAccessible(true);
		} catch (NoSuchFieldException e) {
			fail(e.toString());
		} catch (SecurityException e) {
			fail(e.toString());
		}
	}

	private void assertWizardEvent(UsageEventType type, String wizardClassName, int count) {
		TestUsageReporter reporter = TestUsageReporter.getInstance();
		TestEventRegister register = reporter.getEventRegister();

		register.setCurrentTime(UsageReporterTest.YESTERDAY);
		for (int i = 0; i < count; i++) {
			boolean ok = reporter.countEventSynchronously(type.event(wizardClassName));
			assertFalse(ok);
		}

		register.setCurrentTime(UsageReporterTest.TODAY);
		boolean ok = reporter.countEventSynchronously(type.event(wizardClassName));
		assertTrue(ok);

		register.setCurrentTime(UsageReporterTest.TODAY);
		ok = reporter.countEventSynchronously(type.event(wizardClassName, 1));
		assertFalse(ok);
	}
}