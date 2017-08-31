/*******************************************************************************
 * Copyright (c) 2013-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.internal.environment.eclipse.EclipseUserAgent;

/**
 * @author Andre Dietisheim
 */
public class EclipseUserAgentFake extends EclipseUserAgent {

	private static final String APPLICATION_NAME = "com.jboss.jbds.product";
	private static final String APPLICATION_VERSION = "3.0.1";

	public static final String LOCALE_US = "en_US";

	public static final String PROP_SUN_ARCH_64 = "64";
	public static final String PROP_SUN_ARCH_32 = "32";
	
	public static final String OS_WINDOWS = Platform.OS_WIN32;
	public static final String OS_LINUX = Platform.OS_LINUX;
	public static final String OS_MAC = Platform.OS_MACOSX;
	
	public static final String WINDOWS_NAME = "Windows";
	public static final String WINNT_NAME = "Windows NT";
	public static final String X11_NAME = "X11";
	public static final String LINUX_NAME = "Linux";
	public static final String MACINTOSH_NAME = "Macintosh";
	public static final String INTELMACOSX_NAME = "Intel Mac OS X ";
	
	public static final String VERSION_WIN2000 = "5.0";
	public static final String VERSION_WINXP = "5.1";
	public static final String VERSION_VISTA = "6.0";
	public static final String VERSION_WIN7 = "6.1";
	public static final String VERSION_WIN8 = "6.2";
	public static final String VERSION_MACLEOPARD = "10.5";
	public static final String VERSION_MACSNOWLEOPARD = "10.6";
	public static final String VERSION_LINUX_FEDORA13 = "13";

	public static final String WINDOWS_ARCH_64 = "Win64; x64";
	public static final String LINUX_ARCH_32 = "i686";
	public static final String LINUX_ARCH_64 = "x86_64";
	
	private String applicationName;
	private String applicationVersion;
	private String nl;
	private String os;
	private String osVersion;
	private String sunArchitecture;

	public EclipseUserAgentFake() {
		this(APPLICATION_NAME, APPLICATION_VERSION, LOCALE_US, OS_WINDOWS, VERSION_WIN7, PROP_SUN_ARCH_64);
	}

	public EclipseUserAgentFake(String nl) {
		this(APPLICATION_NAME, APPLICATION_VERSION, nl, OS_WINDOWS, VERSION_WIN7, PROP_SUN_ARCH_64);
	}

	public EclipseUserAgentFake(String nl, String os, String osVersion) {
		this(nl, os, osVersion, PROP_SUN_ARCH_64);
	}

	public EclipseUserAgentFake(String nl, String os, String osVersion, String sunArchitecture) {
		this(APPLICATION_NAME, APPLICATION_VERSION, nl, os, osVersion, sunArchitecture);
	}

	/**
	 * Instantiates a fake of the EclipseUserAgent class
	 * 
	 * @param applicationName
	 *            the name of the applicaiton
	 * @param applicationVersion
	 *            the version of the application
	 * @param nl
	 *            the platform locale as returned from
	 *            <code>Platform.getNL()</code>
	 * @param os
	 *            the os name as return from <code>Platform.getOS()</code>
	 * @param osVersion
	 *            the version of the os as returned from
	 *            <code>System.getProperty("os.name")</code>
	 * 
	 * @see Platform#getNL()
	 * @see Platform#getOS()
	 * @see System#getProperty("os.version")
	 */
	public EclipseUserAgentFake(String applicationName, String applicationVersion, String nl, String os,
			String osVersion, String sunArchitecture) {
		Assert.isTrue(nl != null && nl.indexOf(JAVA_LOCALE_DELIMITER) >= 0,
				"nl parameter must for a java locale string <xx_XX>");
		this.applicationName = applicationName;
		this.applicationVersion = applicationVersion;
		this.nl = nl;
		this.os = os;
		this.osVersion = osVersion;
		this.sunArchitecture = sunArchitecture;
	}

	@Override
	protected String getNL() {
		return nl;
	}

	@Override
	public String getApplicationName() {
		return applicationName;
	}

	@Override
	public String getOS() {
		return os;
	}

	@Override
	public String getJavaArchitecture() {
		return sunArchitecture;
	}

	@Override
	public String getOSVersion() {
		return osVersion;
	}

	@Override
	public String getApplicationVersion() {
		return applicationVersion;
	}
}