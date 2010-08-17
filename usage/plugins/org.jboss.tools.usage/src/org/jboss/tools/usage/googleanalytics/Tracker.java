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
package org.jboss.tools.usage.googleanalytics;

import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;

import org.jboss.tools.usage.ITracker;
import org.jboss.tools.usage.internal.GoogleAnalyticsUrlStrategy;
import org.jboss.tools.usage.internal.HttpGetMethod;

/**
 * Reports (tracks) usage
 * 
 * @author Andre Dietisheim
 * @see based on <a
 *      href="http://jgoogleAnalytics.googlecode.com">http://jgoogleAnalytics.googlecode.com</a>
 */
public class Tracker implements ITracker {

	private IURLBuildingStrategy urlBuildingStrategy = null;
	private HttpGetMethod httpRequest;
	private ILoggingAdapter loggingAdapter;

	public Tracker(IURLBuildingStrategy urlBuildingStrategy, String userAgent, ILoggingAdapter loggingAdapter) {
		this.httpRequest = new HttpGetMethod(userAgent, loggingAdapter);
		this.loggingAdapter = loggingAdapter;
		this.urlBuildingStrategy = urlBuildingStrategy;
	}

	public void trackSynchronously(FocusPoint focusPoint) throws UnsupportedEncodingException {
		loggingAdapter.logMessage("Tracking synchronously focusPoint=" + focusPoint.getContentTitle());
		httpRequest.request(urlBuildingStrategy.build(focusPoint));
	}

	public void trackAsynchronously(FocusPoint focusPoint) {
		loggingAdapter.logMessage("Tracking Asynchronously focusPoint=" + focusPoint.getContentTitle());
		new TrackingThread(focusPoint).start();
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
				loggingAdapter.logMessage(MessageFormat.format("Tracking failed: {0}", e.getMessage()));
			}
		}
	}
}
