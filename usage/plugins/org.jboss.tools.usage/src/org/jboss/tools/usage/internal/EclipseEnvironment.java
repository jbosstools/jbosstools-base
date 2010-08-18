/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import java.text.MessageFormat;
import java.util.Random;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.googleanalytics.IGoogleAnalyticsParameters;
import org.jboss.tools.usage.preferences.IUsageReportPreferenceConstants;
import org.jboss.tools.usage.util.BundleUtils;
import org.jboss.tools.usage.util.PreferencesUtil;
import org.jboss.tools.usage.util.StatusUtils;
import org.jboss.tools.usage.util.BundleUtils.IBundleEntryFilter;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/**
 * @author Andre Dietisheim
 */
public class EclipseEnvironment extends AbstractGoogleAnalyticsParameters implements IGoogleAnalyticsParameters {

	private static final String ECLIPSE_RUNTIME_BULDEID = "org.eclipse.core.runtime";

	private String screenResolution;

	private String screenColorDepth;

	private Random random;

	public EclipseEnvironment(String accountName, String hostName, String referral) {
		super(accountName, hostName, referral);
		this.random = new Random();
		initScreenSettings();
	}

	@Override
	public String getBrowserLanguage() {
		String nl = getNL();
		if (nl == null) {
			return "";
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

	protected void initScreenSettings() {
		final Display display = getDisplay();
		display.syncExec(new Runnable() {
			@Override
			public void run() {
				screenColorDepth = display.getDepth() + SCREENCOLORDEPTH_POSTFIX;

				Rectangle bounds = display.getBounds();
				screenResolution = bounds.width + SCREERESOLUTION_DELIMITER + bounds.height;
			}
		});
	}

	@Override
	public String getScreenResolution() {
		return screenResolution;
	}

	@Override
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

	@Override
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
		String userAgentPattern = "";
		/*
		 * TODO: implement architecture (i686, x86_64 etc.), Windows version, MacOS version etc. 
		 */
		if (Platform.OS_LINUX.equals(os)) {
			return "{0}/{1} (X11; U; Linux i686; {2})";
		} else if (Platform.OS_MACOSX.equals(os)) {
			return "{0}/{1} (Macintosh; U; Intel Mac OS X 10.5; {2})";
		} else if (Platform.OS_WIN32.equals(os)) {
			return "{0}/{1} (Windows; U; Windows NT 6.1; {2})";		
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

	@Override
	public String getUserId() {
		Preferences preferences = PreferencesUtil.getConfigurationPreferences();
		String userId = createIdentifier();
		try {
			if (!preferences.nodeExists(IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID)) {
				preferences.put(IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID, userId);
				preferences.flush();
			} else {
				userId = preferences.get(IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID, userId);
			}
		} catch (BackingStoreException e) {
			StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID, "Could not retrieve {0} from preferences.",
					e, IUsageReportPreferenceConstants.ECLIPSE_INSTANCE_ID);
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

	@Override
	public String getKeyword() {
		JBossBundleGroups jbossBundleGroups = new JBossBundleGroups();
		IBundleEntryFilter jbossToolsFilter = new BundleUtils.BundleSymbolicNameFilter("org\\.jboss\\.tools.+");
		IBundleEntryFilter compositeFilter = new BundleUtils.CompositeFilter(
				jbossToolsFilter
				, jbossBundleGroups );
		BundleUtils.getBundles(compositeFilter, JBossToolsUsageActivator.getDefault().getBundle().getBundleContext());
		
		return bundleGroupsToKeywordString(jbossBundleGroups);
	}
	
	private String bundleGroupsToKeywordString(JBossBundleGroups jbossBundleGroups) {
		char delimiter = '-';
		StringBuilder builder = new StringBuilder();
		for (String bundleGroupId : jbossBundleGroups.getBundleGroupIds()) {
			builder.append(bundleGroupId)
			.append(delimiter);
		}
		return builder.toString();
	}
}
