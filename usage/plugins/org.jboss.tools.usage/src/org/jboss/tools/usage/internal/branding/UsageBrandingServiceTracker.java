/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.branding;

import org.jboss.tools.usage.branding.IUsageBranding;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;

public class UsageBrandingServiceTracker extends ServiceTracker implements IUsageBranding {

	@Override
	public Object addingService(ServiceReference reference) {
		System.err.println(reference);
		return super.addingService(reference);
	}

	@Override
	public void removedService(ServiceReference reference, Object service) {
		System.err.println(reference);
		super.removedService(reference, service);
	}

	JBossToolsUsageBranding jbossBranding = new JBossToolsUsageBranding();

	public UsageBrandingServiceTracker(BundleContext context) {
		super(context, IUsageBranding.class.getName(), null);
	}

	public String getPreferencesDescription() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getPreferencesDescription();
		} else {
			return jbossBranding.getPreferencesDescription();
		}
	}

	public String getPreferencesAllowReportingCheckboxLabel() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getPreferencesAllowReportingCheckboxLabel();
		} else {
			return jbossBranding.getPreferencesAllowReportingCheckboxLabel();
		}
	}

	public String getStartupAllowReportingTitle() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getStartupAllowReportingTitle();
		} else {
			return jbossBranding.getStartupAllowReportingTitle();
		}
	}

	public String getStartupAllowReportingCheckboxLabel() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getStartupAllowReportingCheckboxLabel();
		} else {
			return jbossBranding.getStartupAllowReportingCheckboxLabel();
		}
	}

	public String getStartupAllowReportingMessage() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getStartupAllowReportingMessage();
		} else {
			return jbossBranding.getStartupAllowReportingMessage();
		}
	}

	public String getStartupAllowReportingDetailLink() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getStartupAllowReportingDetailLink();
		} else {
			return jbossBranding.getStartupAllowReportingDetailLink();
		}

	}

	public String getGlobalRemotePropertiesUrl() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getGlobalRemotePropertiesUrl();
		} else {
			return jbossBranding.getGlobalRemotePropertiesUrl();
		}
	}

	public String getGoogleAnalyticsAccount() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getGoogleAnalyticsAccount();
		} else {
			return jbossBranding.getGoogleAnalyticsAccount();
		}
	}

	public String getGoogleAnalyticsReportingHost() {
		IUsageBranding service = (IUsageBranding) getService();
		if (service != null) {
			return service.getGoogleAnalyticsReportingHost();
		} else {
			return jbossBranding.getGoogleAnalyticsReportingHost();
		}
	}
}
