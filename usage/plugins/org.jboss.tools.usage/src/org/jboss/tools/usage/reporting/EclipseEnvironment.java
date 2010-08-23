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
package org.jboss.tools.usage.reporting;

import java.text.MessageFormat;
import java.util.Random;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.googleanalytics.AbstractGoogleAnalyticsParameters;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.preferences.IUsageReportPreferenceConstants;
import org.jboss.tools.usage.util.BundleUtils;
import org.jboss.tools.usage.util.PreferencesUtils;
import org.jboss.tools.usage.util.BundleUtils.IBundleEntryFilter;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironment extends AbstractGoogleAnalyticsParameters implements IGoogleAnalyticsParameters {

	private static final String USERAGENT_WIN = "{0}/{1} (Windows; U; Windows NT 6.1; {2})"; //$NON-NLS-1$
	private static final String USERAGENT_MAC = "{0}/{1} (Macintosh; U; Intel Mac OS X 10.5; {2})"; //$NON-NLS-1$
	private static final String USERAGENT_LINUX = "{0}/{1} (X11; U; Linux i686; {2})"; //$NON-NLS-1$
	private static final char BUNDLE_GROUP_DELIMITER = '-';
	private static final String JBOSS_TOOLS_BUNDLES_PREFIX = "org\\.jboss\\.tools.+"; //$NON-NLS-1$
	private static final String ECLIPSE_RUNTIME_BULDEID = "org.eclipse.core.runtime"; //$NON-NLS-1$

	private String screenResolution;
	private String screenColorDepth;
	private Random random;
	private IEclipsePreferences preferences;
	private String firstVisit;
	private String lastVisit;
	private String currentVisit;
	private long visitCount;

	public EclipseEnvironment(String accountName, String hostName, String referral, IEclipsePreferences preferences) {
		super(accountName, hostName, referral);
		this.random = new Random();
		this.preferences = preferences;
		initScreenSettings();
		initVisits();
	}

	protected void initScreenSettings() {
		final Display display = getDisplay();
		display.syncExec(new Runnable() {

			public void run() {
				screenColorDepth = display.getDepth() + SCREENCOLORDEPTH_POSTFIX;

				Rectangle bounds = display.getBounds();
				screenResolution = bounds.width + SCREERESOLUTION_DELIMITER + bounds.height;
			}
		});
	}

	private void initVisits() {
		String currentTime = String.valueOf(System.currentTimeMillis());
		this.currentVisit = currentTime;
		this.firstVisit = preferences.get(IUsageReportPreferenceConstants.FIRST_VISIT, null);
		if (firstVisit == null) {
			this.firstVisit = currentTime;
			preferences.put(IUsageReportPreferenceConstants.FIRST_VISIT, firstVisit);
		}
		lastVisit = preferences.get(IUsageReportPreferenceConstants.LAST_VISIT, currentTime);
		visitCount = preferences.getLong(IUsageReportPreferenceConstants.VISIT_COUNT, 1);
	}

	public String getBrowserLanguage() {
		String nl = getNL();
		if (nl == null) {
			return ""; //$NON-NLS-1$
		}

		int indexOf = nl.indexOf(JAVA_LOCALE_DELIMITER); //$NON-NLS-1$
		if (indexOf <= 0) {
			return nl;
		}

		StringBuilder builder = new StringBuilder();
		builder.append(nl.substring(0, indexOf));
		builder.append(BROWSER_LOCALE_DELIMITER);
		builder.append(nl.substring(indexOf + 1));
		return builder.toString();
	}

	protected String getNL() {
		return Platform.getNL(); //$NON-NLS-1$
	}

	public String getScreenResolution() {
		return screenResolution;
	}

	public String getScreenColorDepth() {
		return screenColorDepth;
	}

	protected Display getDisplay() {
		if (PlatformUI.isWorkbenchRunning()) {
			return PlatformUI.getWorkbench().getDisplay();
		}

		Display display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}
		return display;
	}

	public String getUserAgent() {

		String productId = getApplicationName();
		String productVersion = getApplicationVersion();
		String browserLanguage = getBrowserLanguage();

		/**
		 * Google API for android: this.userAgent = String.format(
		 * "GoogleAnalytics/%s (Linux; U; Android %s; %s-%s; %s; Build/%s)" ,
		 * new Object[] { "1.0" , Build.VERSION.RELEASE ,
		 * (localLocale.getLanguage() != null) ?
		 * localLocale.getLanguage().toLowerCase() : "en" ,
		 * (localLocale.getCountry() != null) ?
		 * localLocale.getCountry().toLowerCase() : "" , Build.MODEL, Build.ID
		 * });
		 */

		return MessageFormat.format(
				getUserAgentPattern(getOS())
				, productId
				, productVersion
				, browserLanguage
				);
	}

	private String getUserAgentPattern(String os) {
		String userAgentPattern = ""; //$NON-NLS-1$
		/*
		 * TODO: implement architecture (i686, x86_64 etc.), Windows version, MacOS version etc. 
		 */
		if (Platform.OS_LINUX.equals(os)) {
			return USERAGENT_LINUX; //$NON-NLS-1$
		} else if (Platform.OS_MACOSX.equals(os)) {
			return USERAGENT_MAC; //$NON-NLS-1$
		} else if (Platform.OS_WIN32.equals(os)) {
			return USERAGENT_WIN; //$NON-NLS-1$
		}
		return userAgentPattern;
	}

	protected String getOS() {
		return Platform.getOS();
	}

	protected String getApplicationName() {
		return getApplicationBundle().getSymbolicName();
	}

	protected String getApplicationVersion() {
		String fullVersion = getApplicationBundle().getVersion().toString();
		int productVersionStart = fullVersion.lastIndexOf(VERSION_DELIMITER);
		if (productVersionStart > 0) {
			return fullVersion.substring(0, productVersionStart);
		} else {
			return fullVersion;
		}
	}

	/**
	 * Returns the bundle that launched the application that this class runs in.
	 * 
	 * @return the defining bundle
	 */
	private Bundle getApplicationBundle() {
		IProduct product = Platform.getProduct();
		if (product != null) {
			return product.getDefiningBundle();
		} else {
			return Platform.getBundle(ECLIPSE_RUNTIME_BULDEID);
		}
	}

	public String getUserId() {
		String userId = preferences.get(IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID, null);
		if (userId == null) {
			userId = createIdentifier();
			preferences.put(IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID, userId);
			PreferencesUtils.checkedSavePreferences(preferences, JBossToolsUsageActivator.getDefault(), ReportingMessages.EclipseEnvironment_Error_SavePreferences);
		}
		return userId;
	}

	/**
	 * Creates an unique identifier.
	 * 
	 * @return the identifier
	 */
	private String createIdentifier() {
		StringBuilder builder = new StringBuilder();
		builder.append(Math.abs(random.nextLong()));
		builder.append(System.currentTimeMillis());
		return builder.toString();
	}

	public String getKeyword() {
		JBossBundleGroups jbossBundleGroups = new JBossBundleGroups();
		IBundleEntryFilter jbossToolsFilter = new BundleUtils.BundleSymbolicNameFilter(JBOSS_TOOLS_BUNDLES_PREFIX);
		IBundleEntryFilter compositeFilter = new BundleUtils.CompositeFilter(
				jbossToolsFilter
				, jbossBundleGroups);
		BundleUtils.getBundles(compositeFilter, getBundles());

		return bundleGroupsToKeywordString(jbossBundleGroups);
	}

	protected Bundle[] getBundles() {
		return JBossToolsUsageActivator.getDefault().getBundle().getBundleContext().getBundles();
	}

	private String bundleGroupsToKeywordString(JBossBundleGroups jbossBundleGroups) {
		char delimiter = BUNDLE_GROUP_DELIMITER;
		StringBuilder builder = new StringBuilder();
		for (String bundleGroupId : jbossBundleGroups.getBundleGroupIds()) {
			builder.append(bundleGroupId)
					.append(delimiter);
		}
		return builder.toString();
	}

	public String getCurrentVisit() {
		return currentVisit;
	}

	public String getFirstVisit() {
		return firstVisit;
	}

	public String getLastVisit() {
		return lastVisit;
	}

	public long getVisitCount() {
		return visitCount;
	}

	public void visit() {
		lastVisit = currentVisit;
		preferences.put(IUsageReportPreferenceConstants.LAST_VISIT, lastVisit);
		currentVisit = String.valueOf(System.currentTimeMillis());
		visitCount++;
		preferences.putLong(IUsageReportPreferenceConstants.VISIT_COUNT, visitCount);
		PreferencesUtils.checkedSavePreferences(preferences, JBossToolsUsageActivator.getDefault(), ReportingMessages.EclipseEnvironment_Error_SavePreferences);
	}
}
