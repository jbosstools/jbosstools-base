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
package org.jboss.tools.usage.test.fakes;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.googleanalytics.IUserAgent;
import org.jboss.tools.usage.reporting.ReportingEclipseEnvironment;

/**
 * @author Andre Dietisheim
 */
public class ReportingEclipseEnvironmentFake extends ReportingEclipseEnvironment {
	
	public static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";
	public static final String HOSTNAME = "jboss.org";
	public static final String JAVA_VERSION = "1.6.0_20";

	private String javaVersion;

	public ReportingEclipseEnvironmentFake() {
		this(GANALYTICS_ACCOUNTNAME, HOSTNAME, JAVA_VERSION, new EclipsePreferencesFake());
	}

	public ReportingEclipseEnvironmentFake(IEclipsePreferences preferences) {
		this(GANALYTICS_ACCOUNTNAME, HOSTNAME, JAVA_VERSION, preferences);
	}

	public ReportingEclipseEnvironmentFake(String accountName, String hostName) {
		this(accountName, hostName, JAVA_VERSION, new EclipsePreferencesFake());
	}

	public ReportingEclipseEnvironmentFake(String accountName, String hostName, String javaVersion,
			IEclipsePreferences preferences) {
		super(accountName, hostName, preferences);
		this.javaVersion = javaVersion;
	}

	@Override
	protected void initScreenSettings() {
		// do not access swt/display
	}

	@Override
	protected IUserAgent createEclipseUserAgent() {
		return new EclipseUserAgentFake();
	}

	@Override
	public String getScreenResolution() {
		return 1920 + SCREERESOLUTION_DELIMITER + 1080;
	}

	@Override
	public String getScreenColorDepth() {
		return 24 + SCREENCOLORDEPTH_POSTFIX;
	}

	public String getFlashVersion() {
		return javaVersion;
	}
}
