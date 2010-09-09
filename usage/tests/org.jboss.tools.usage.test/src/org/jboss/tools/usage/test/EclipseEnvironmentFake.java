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

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.jboss.tools.usage.reporting.EclipseEnvironment;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironmentFake extends EclipseEnvironment {

	public static final String GANALYTICS_ACCOUNTNAME = "UA-17645367-1";
	public static final String HOSTNAME = "jboss.org";
	public static final String REFERRAL = "0";
	public static final String LOCALE_US = "en_US";
	public static final String JAVA_VERSION = "1.6.0_20";

	private String locale;
	private String os;
	private String javaVersion;

	public EclipseEnvironmentFake() {
		this(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL, Platform.OS_LINUX, LOCALE_US, JAVA_VERSION);
	}

	public EclipseEnvironmentFake(String platform) {
		this(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL, platform, LOCALE_US, JAVA_VERSION);
	}

	public EclipseEnvironmentFake(IEclipsePreferences preferences) {
		this(GANALYTICS_ACCOUNTNAME, HOSTNAME, REFERRAL, Platform.OS_LINUX, LOCALE_US, JAVA_VERSION, preferences);
	}

	public EclipseEnvironmentFake(String accountName, String hostName, String referral, String os, String locale) {
		this(accountName, hostName, referral, os, locale, JAVA_VERSION);
	}
			public EclipseEnvironmentFake(String accountName, String hostName, String referral, String os, String locale,
			String javaVersion, IEclipsePreferences preferences) {
		super(accountName, hostName, referral, preferences);
		this.os = os;
		this.javaVersion = javaVersion;
		this.locale = locale;
	}

	public EclipseEnvironmentFake(String accountName, String hostName, String referral, String os, String javaVersion, String locale) {
		this(accountName, hostName, referral, os, locale, javaVersion, new EclipsePreferencesFake());
	}

	@Override
	protected void initScreenSettings() {
		// do not access swt/display
	}

	@Override
	public String getScreenResolution() {
		return 1920 + SCREERESOLUTION_DELIMITER + 1080;
	}

	@Override
	public String getScreenColorDepth() {
		return 24 + SCREENCOLORDEPTH_POSTFIX;
	}

	@Override
	protected String getApplicationName() {
		return "com.jboss.jbds.product";
	}

	@Override
	protected String getNL() {
		return locale;
	}

	@Override
	protected String getOS() {
		return os;
	}

	@Override
	protected String getApplicationVersion() {
		return "3.0.1";
	}
	
	protected String getJavaVersion() {
		return javaVersion;
	}
}
