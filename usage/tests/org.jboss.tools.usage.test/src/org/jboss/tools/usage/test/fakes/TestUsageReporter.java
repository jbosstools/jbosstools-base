/*******************************************************************************
 * Copyright (c) 2014-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.event.UsageReporter;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.environment.eclipse.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.event.RequestType;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;

/**
 * @author Alexey Kazakov
 */
public class TestUsageReporter extends UsageReporter {

	private static final TestUsageReporter INSTANCE = new TestUsageReporter();

	private IJBossToolsEclipseEnvironment environment;
	private GlobalUsageSettings testSettings;

	protected TestUsageReporter() {
	}

	public static TestUsageReporter getInstance() {
		return INSTANCE;
	}

	@Override
	protected IJBossToolsEclipseEnvironment getEnvironment() {
	    if (environment == null) {
	        environment = new ReportingEclipseEnvironmentFake(ReportingEclipseEnvironmentFake.JAVA_VERSION,
	                new EclipsePreferencesFake(),
	                new EclipseUserAgentFake());
	    }
	    return environment;
	}

	@Override
	public void registerEvent(final UsageEventType type) {
		registerEventSynchronously(type);
	}

	public int registerEventSynchronously(final UsageEventType type) {
		getEventRegister().registerEvent(type);
		return checkCountEventInternal(type);
	}

	public boolean trackEventSynchronously(String pagePath, String title, UsageEvent event, RequestType type, boolean startNewVisitSession) {
		return trackEventInternal(pagePath, title, event, type, startNewVisitSession);
	}

	public boolean trackEventSynchronously(UsageEvent event) {
		return trackEventInternal(event);
	}

	public boolean countEventSynchronously(UsageEvent event) {
		return super.countEventInternal(event);
	}

	public int checkCountEventSynchronously(UsageEventType type) {
		return super.checkCountEventInternal(type);
	}

	public int trackCountEventsSynchronously() {
		return super.trackCountEventsInternal();
	}

	@Override
	public TestEventRegister getEventRegister() {
		return TestEventRegister.getInstance();
	}

	@Override
	protected synchronized GlobalUsageSettings getGlobalUsageSettings() {
		if(testSettings==null) {
			testSettings = new GlobalUsageSettings(JBossToolsUsageActivator.getDefault()) {
				@Override
				protected boolean isInstanceReportingEnabled() {
					return true;
				}
			};
		}
		return testSettings;
	}

	@Override
	protected boolean isPreferencesEnabled() {
		return true;
	}
}