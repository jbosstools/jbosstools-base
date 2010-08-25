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
package org.jboss.tools.usage;

import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;

/**
 * Reports (tracks) usage
 * 
 * @author Andre Dietisheim
 * @see based on <a
 *      href="http://jgoogleAnalytics.googlecode.com">http://jgoogleAnalytics
 *      .googlecode.com</a>
 */
public class Tracker implements ITracker {

	private IURLBuildingStrategy urlBuildingStrategy = null;
	private IHttpGetRequest httpRequest;
	private ILoggingAdapter loggingAdapter;

	public Tracker(IURLBuildingStrategy urlBuildingStrategy, IHttpGetRequest httpGetRequest, ILoggingAdapter loggingAdapter) {
		this.httpRequest = httpGetRequest;
		this.loggingAdapter = loggingAdapter;
		this.urlBuildingStrategy = urlBuildingStrategy;
	}

	public void trackSynchronously(FocusPoint focusPoint) {
		loggingAdapter
				.logMessage(MessageFormat.format(UsageMessages.Tracker_Synchronous, focusPoint.getContentTitle()));
		try {
			httpRequest.request(getTrackingUrl(focusPoint));
		} catch (Exception e) {
			loggingAdapter.logMessage(MessageFormat.format(UsageMessages.Tracker_Error, e.getMessage()));
		}
	}

	protected String getTrackingUrl(FocusPoint focusPoint) throws UnsupportedEncodingException {
		return urlBuildingStrategy.build(focusPoint);
	}

	public void trackAsynchronously(FocusPoint focusPoint) {
		loggingAdapter.logMessage(MessageFormat
				.format(UsageMessages.Tracker_Asynchronous, focusPoint.getContentTitle()));
		new Thread(new TrackingRunnable(focusPoint)).start();
	}

	private class TrackingRunnable implements Runnable {
		private FocusPoint focusPoint;

		private TrackingRunnable(FocusPoint focusPoint) {
			this.focusPoint = focusPoint;
		}

		public void run() {
			try {
				httpRequest.request(getTrackingUrl(focusPoint));
			} catch (Exception e) {
				loggingAdapter.logMessage(MessageFormat.format(UsageMessages.Tracker_Error, e.getMessage()));
			}
		}
	}
}
