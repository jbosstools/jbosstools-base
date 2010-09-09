package org.jboss.tools.usage.test;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.reporting.EclipseUserAgent;

public class EclipseUserAgentFake extends EclipseUserAgent {

	private static final String APPLICATION_NAME = "com.jboss.jbds.product";
	private static final String APPLICATION_VERSION = "3.0.1";
	public static final String LOCALE_US = "en_US";

	private String applicationName;
	private String applicationVersion;
	private String nl;
	private String os;

	public EclipseUserAgentFake() {
		this(APPLICATION_NAME, APPLICATION_VERSION, LOCALE_US, Platform.OS_LINUX);
	}
	
	public EclipseUserAgentFake(String nl) {
		this(APPLICATION_NAME, APPLICATION_VERSION, nl, Platform.OS_LINUX);
	}

	public EclipseUserAgentFake(String nl, String os) {
		this(APPLICATION_NAME, APPLICATION_VERSION, nl, os);
	}

	public EclipseUserAgentFake(String applicationName, String applicationVersion, String nl, String os) {
		Assert.isTrue(nl != null && nl.indexOf(JAVA_LOCALE_DELIMITER) >= 0, "nl parameter must for a java locale string <xx_XX>");
		this.applicationName = applicationName;
		this.applicationVersion = applicationVersion;
		this.nl = nl;
		this.os = os;
	}

	@Override
	protected String getNL() {
		return nl;
	}

	@Override
	protected String getApplicationName() {
		return applicationName;
	}

	@Override
	protected String getOS() {
		return os;
	}

	@Override
	protected String getApplicationVersion() {
		return applicationVersion;
	}
}