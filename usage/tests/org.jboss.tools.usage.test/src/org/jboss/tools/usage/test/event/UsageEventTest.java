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
import static org.junit.Assert.fail;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.junit.Test;
import org.osgi.framework.Version;

/**
 * @author Alexey Kazakov
 */
public class UsageEventTest {

	@Test
	public void testEventTypeConstructor() {
		UsageEventType type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test-action", "test-label-description", "test-value-description");
		assertEquals("usage", type.getComponentName());
		assertEquals("usage", type.getCategoryName());

		Version version = JBossToolsUsageActivator.getDefault().getBundle().getVersion();
		String componentVersion = "" + version.getMajor() + "." + version.getMinor() + "." + version.getMicro();
		assertEquals(componentVersion, type.getComponentVersion());
	}

	@Test
	public void testEventTypeWithoutValueDescription() {
		UsageEventType type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test action", "test label description");
		assertEquals("test-action", type.getActionName());
		try {
			new UsageEvent(type, "test-label", 1);
		} catch (IllegalArgumentException e) {
			return;
		}
		fail("Usage event doesn't correspond to its type");
	}

	@Test
	public void testEventWithoutLabelDescription() {
		UsageEventType type = new UsageEventType(JBossToolsUsageActivator.getDefault(), "test-action");
		try {
			new UsageEvent(type, "test-label");
		} catch (IllegalArgumentException e) {
			return;
		}
		fail("Usage event doesn't correspond to its type");
	}
}