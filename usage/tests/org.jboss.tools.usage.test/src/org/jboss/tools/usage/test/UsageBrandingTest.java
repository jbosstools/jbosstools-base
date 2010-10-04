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
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;

import org.jboss.tools.usage.branding.IUsageBranding;
import org.jboss.tools.usage.internal.branding.JBossToolsUsageBranding;
import org.jboss.tools.usage.internal.branding.UsageBrandingMediator;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * @author Andre Dietisheim
 */
public class UsageBrandingTest {

	private static final String DEFAULT_TITLE = "default";
	private static final String SERVICE_TITLE = "service";

	@Test
	public void getsTitleFromDefaultIfServiceNotPresent() {
		DefaultUsageBrandingFake defaultBranding = new DefaultUsageBrandingFake();
		UsageBrandingMediator brandingMediator = new UsageBrandingMediator(defaultBranding
				, JBossToolsUsageTestActivator.getDefault().getBundle().getBundleContext());
		assertEquals(DEFAULT_TITLE, brandingMediator.getStartupAllowReportingTitle());
	}

	@Test
	public void getsTitleFromServiceIfPresent() {
		ServiceRegistration registration = registerBrandingService(new UsageBrandingServiceFake());
		DefaultUsageBrandingFake defaultBranding = new DefaultUsageBrandingFake();
		UsageBrandingMediator brandingMediator = new UsageBrandingMediator(defaultBranding
				, JBossToolsUsageTestActivator.getDefault().getBundle().getBundleContext());
		brandingMediator.open();
		try {
			assertEquals(SERVICE_TITLE, brandingMediator.getStartupAllowReportingTitle());
		} finally {
			deregisterBrandingService(registration.getReference());
		}
	}

	private ServiceRegistration registerBrandingService(IUsageBranding brandingService) {
		BundleContext context = JBossToolsUsageTestActivator.getDefault().getBundle().getBundleContext();
		return context.registerService(IUsageBranding.class.getName(), brandingService, null);
	}

	private void deregisterBrandingService(ServiceReference reference) {
		BundleContext context = JBossToolsUsageTestActivator.getDefault().getBundle().getBundleContext();
		context.ungetService(reference);
	}

	private class DefaultUsageBrandingFake extends UsageBrandingServiceFake {

		public String getStartupAllowReportingTitle() {
			return DEFAULT_TITLE;
		}
	}

	private class UsageBrandingServiceFake extends JBossToolsUsageBranding {

		public String getStartupAllowReportingTitle() {
			return SERVICE_TITLE;
		}
	}
}
