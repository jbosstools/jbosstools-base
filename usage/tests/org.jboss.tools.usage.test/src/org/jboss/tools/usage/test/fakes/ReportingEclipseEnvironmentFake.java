/*******************************************************************************
 * Copyright (c) 2010-2017 Red Hat, Inc.
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
import org.jboss.tools.usage.internal.UsageConstants;
import org.jboss.tools.usage.internal.environment.eclipse.IEclipsePlatform;
import org.jboss.tools.usage.internal.environment.eclipse.JBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.environment.eclipse.LinuxSystem;
import org.jboss.tools.usage.test.fakes.LinuxSystemFake.ReleaseFile;

/**
 * @author Andre Dietisheim
 */
public class ReportingEclipseEnvironmentFake extends JBossToolsEclipseEnvironment {

	public static final String JAVA_VERSION = "1.6.0_20";

	private String javaVersion;

	public ReportingEclipseEnvironmentFake() {
		this(new EclipsePreferencesFake());
	}

	public ReportingEclipseEnvironmentFake(IEclipsePlatform userAgent) {
		this(new EclipsePreferencesFake(), userAgent);
	}
	
	public ReportingEclipseEnvironmentFake(IEclipsePreferences preferences, IEclipsePlatform userAgent) {
		this( JAVA_VERSION, preferences, userAgent);
	}

	public ReportingEclipseEnvironmentFake(IEclipsePreferences preferences) {
		this( JAVA_VERSION, preferences, new EclipseUserAgentFake());
	}

	public ReportingEclipseEnvironmentFake(String javaVersion,
			IEclipsePreferences preferences, IEclipsePlatform platform) {
		super(preferences, platform);
		this.javaVersion = javaVersion;
	}

	@Override
	protected void initScreenSettings() {
		// do not access swt/display
	}

	@Override
	public String getScreenResolution() {
		return 1920 + UsageConstants.SCREERESOLUTION_DELIMITER + 1080;
	}

	@Override
	public String getScreenColorDepth() {
		return 24 + UsageConstants.SCREENCOLORDEPTH_POSTFIX;
	}

	public String getFlashVersion() {
		return javaVersion;
	}

	@Override
	protected String getLinuxDistroNameAndVersion() {
		return new LinuxSystemFake(new ReleaseFile(LinuxSystem.INSTANCE.FEDORA.getReleaseFilePath(), "Fedora release 13 (Goddard)")).getDistroNameAndVersion();
	}
	
	
}
