/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import org.jboss.tools.usage.branding.IUsageBranding;
import org.jboss.tools.usage.internal.branding.JBossToolsUsageBranding;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsTestBranding extends JBossToolsUsageBranding implements IUsageBranding {

	public static final String GOOGLE_ANALYTICS_TEST_ACCOUNT = "UA-41739937-1";
	public static final String REPORTING_HOST = "jboss.org";

	@Override
	public String getGoogleAnalyticsAccount() {
		return GOOGLE_ANALYTICS_TEST_ACCOUNT;
	}

	@Override
	public String getGoogleAnalyticsReportingHost() {
		return REPORTING_HOST;
	}

}
