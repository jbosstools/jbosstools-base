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
package org.jboss.tools.usage.googleanalytics.eclipse;

import java.text.MessageFormat;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.usage.googleanalytics.IUserAgent;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class EclipseUserAgent implements IUserAgent {

	public static final char JAVA_LOCALE_DELIMITER = '_';

	private static final String ECLIPSE_RUNTIME_BULDEID = "org.eclipse.core.runtime"; //$NON-NLS-1$

	private static final String USERAGENT_WIN = "{0}/{1} (Windows; U; Windows NT {2}; {3})"; //$NON-NLS-1$
	private static final String USERAGENT_MAC = "{0}/{1} (Macintosh; U; Intel Mac OS X {2}; {3})"; //$NON-NLS-1$
	private static final String USERAGENT_LINUX = "{0}/{1} (X11; U; Linux i686; {2})"; //$NON-NLS-1$

	public static final char VERSION_DELIMITER = '.'; //$NON-NLS-1$

	private static final String PROP_OS_VERSION = "os.version"; //$NON-NLS-1$

	private String browserLanguage;

	private String createBrowserLanguage() {
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
		return Platform.getNL();
	}

	public String getBrowserLanguage() {
		if (browserLanguage == null) {
			browserLanguage = createBrowserLanguage();
		}
		return browserLanguage;
	}

	public String toString() {
		String productId = getApplicationName();
		String productVersion = getApplicationVersion();

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
				, getOSVersion()
				, getBrowserLanguage()
				);
	}

	protected String getOS() {
		return Platform.getOS();
	}

	/**
	 * Returns the version of the operating system this jre is currently running
	 * on.
	 * 
	 * @return the os version
	 * 
	 * @see <a href="http://lopica.sourceforge.net/os.html">list of os versions
	 *      and os names</a> return by the j
	 */
	protected Object getOSVersion() {
		return System.getProperty(PROP_OS_VERSION);
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
}
