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
package org.jboss.tools.usage.tracker.internal;

import org.jboss.tools.foundation.core.usage.IUsageTracker;
import org.jboss.tools.usage.internal.reporting.UsageReport;

public class UsageTracker implements IUsageTracker{
	private UsageReport report;

	public UsageTracker() {
		report = new UsageReport();
	}

	public void sendDailyEvent(String eventCategory, String eventAction, String eventLabel) {
		//report.report(eventCategory, eventAction, eventLabel);
	}

	public void sendLiveEvent(String eventCategory, String eventAction, String eventLabel) {
		report.report(eventCategory, eventAction, eventLabel);
	}

}
