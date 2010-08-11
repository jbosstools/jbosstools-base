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

import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;

import org.jboss.tools.usage.IUsageTracker;

/**
 * Main class for tracking google analytics data.
 * 
 * @author Siddique Hameed
 * @author Andre Dietisheim
 * @version 0.2
 * @see : <a
 *      href="http://JGoogleAnalytics.googlecode.com">http://JGoogleAnalytics
 *      .googlecode.com</a>
 */

public class JGoogleAnalyticsTracker implements IUsageTracker {

	private IURLBuildingStrategy urlBuildingStrategy = null;
	private HttpGetMethod httpRequest;
	private ILoggingAdapter loggingAdapter;

	/**
	 * Simple constructor passing the application name & google analytics
	 * tracking code.
	 * 
	 * @param appName
	 *            Application name (For ex: "LibraryFinder")
	 */
	public JGoogleAnalyticsTracker(IGoogleAnalyticsParameters googleParameters) {
		this.httpRequest = new HttpGetMethod(googleParameters.getUserAgent());
		this.urlBuildingStrategy = new GoogleAnalyticsUrlStrategy(googleParameters);
	}

	/**
	 * Setter injection for LoggingAdpater. You can hook up log4j, System.out or
	 * any other loggers you want.
	 * 
	 * @param loggingAdapter
	 *            implemented instance of LoggingAdapter
	 */

	public void setLoggingAdapter(ILoggingAdapter loggingAdapter) {
		this.loggingAdapter = loggingAdapter;
		httpRequest.setLoggingAdapter(loggingAdapter);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.usage.analytics.jgoogleanalytics.UsageAnalyticsTracker#trackSynchronously(org.jboss.tools.usage.analytics.jgoogleanalytics.FocusPoint)
	 */

	public void trackSynchronously(FocusPoint focusPoint) throws UnsupportedEncodingException {
		logMessage("Tracking synchronously focusPoint=" + focusPoint.getContentTitle());
		httpRequest.request(urlBuildingStrategy.build(focusPoint));
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.usage.analytics.jgoogleanalytics.UsageAnalyticsTracker#trackAsynchronously(org.jboss.tools.usage.analytics.jgoogleanalytics.FocusPoint)
	 */

	public void trackAsynchronously(FocusPoint focusPoint) {
		logMessage("Tracking Asynchronously focusPoint=" + focusPoint.getContentTitle());
		new TrackingThread(focusPoint).start();
	}

	private void logMessage(String message) {
		if (loggingAdapter != null) {
			loggingAdapter.logMessage(message);
		}
	}

	private class TrackingThread extends Thread {
		private FocusPoint focusPoint;

		public TrackingThread(FocusPoint focusPoint) {
			this.focusPoint = focusPoint;
			this.setPriority(Thread.MIN_PRIORITY);
		}

		public void run() {
			try {
				httpRequest.request(urlBuildingStrategy.build(focusPoint));
			} catch (UnsupportedEncodingException e) {
				logMessage(MessageFormat.format("Tracking failed: {0}", e.getMessage()));
			}
		}
	}
}
