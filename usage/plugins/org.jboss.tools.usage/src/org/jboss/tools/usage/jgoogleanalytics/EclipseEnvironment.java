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
package org.jboss.tools.usage.jgoogleanalytics;

import java.util.Random;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.preferences.IUsageReportPreferenceConstants;
import org.jboss.tools.usage.util.PreferencesUtil;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class EclipseEnvironment extends AbstractGoogleAnalyticsParameters implements IGoogleAnalyticsParameters {

	private static final String SCREERESOLUTION_DELIMITER = "x";

	private static final String ECLIPSE_RUNTIME_BULDEID = "org.eclipse.core.runtime";

	private static final char VERSION_DELIMITER = '.';

	private static final String SCREENCOLORDEPTH_POSTFIX = "-bit";

	private static final char JAVA_LOCALE_DELIMITER = '_';

	private static final char BROWSER_LOCALE_DELIMITER = '-';

	private String screenResolution;

	private String screenColorDepth;

	private Random random;

	public EclipseEnvironment(String accountName, String referral) {
		super(accountName, referral);
		this.random = new Random();
		initScreenSettings(getDisplay());
	}

	@Override
	public String getScreenResolution() {
		return screenResolution;
	}

	@Override
	public String getHostname() {
		/* TODO implement */
		return "jboss.org";
	}

	@Override
	public String getBrowserLanguage() {
		String nl = Platform.getNL(); //$NON-NLS-1$
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
		builder.append(nl.substring(indexOf + 1).toLowerCase());
		return builder.toString();
	}

	private void initScreenSettings(final Display display) {
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
	public String getScreenColorDepth() {
		return screenColorDepth;
	}

	private Display getDisplay() {
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

		String productId = getDefiningBundle().getSymbolicName();
		String productVersion = getProductVersion();
		String eclipseVersion = getEclipseVersion();
		String windowSystem = Platform.getWS();
		String os = Platform.getOS();
		String architecture = Platform.getOSArch();
		String browserLanguage = Platform.getNL();
		String buildId = getBuildId();

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

		// return
		// MessageFormat.format("{0}/{1} ({2}; U; {3} {4}; {5}) {6} Eclipse/{7}"
		// , productId
		// , productVersion
		// , windowSystem
		// , os
		// , architecture
		// , browserLanguage
		// , buildId
		// , eclipseVersion);
		return "EclipseEnvironment/3.0.1";
	}

	private String getEclipseVersion() {
		return Platform.getBundle(ECLIPSE_RUNTIME_BULDEID).getVersion().toString();
	}

	private String getBuildId() {
		String fullVersion = getDefiningBundle().getVersion().toString();
		int buildIdStart = fullVersion.lastIndexOf(VERSION_DELIMITER);
		if (buildIdStart > 0) {
			return fullVersion.substring(buildIdStart + 1);
		} else {
			return fullVersion;
		}
	}

	private String getProductVersion() {
		String fullVersion = getDefiningBundle().getVersion().toString();
		int productVersionStart = fullVersion.lastIndexOf(VERSION_DELIMITER);
		if (productVersionStart > 0) {
			return fullVersion.substring(0, productVersionStart);
		} else {
			return fullVersion;
		}
	}

	private Bundle getDefiningBundle() {
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
		String userId = getIdentifier();
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

	private String getIdentifier() {
		StringBuilder builder = new StringBuilder();
		builder.append(random.nextLong());
		builder.append(System.currentTimeMillis());
		return builder.toString();
	}
}
