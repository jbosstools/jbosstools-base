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

import java.io.UnsupportedEncodingException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.IHttpGetRequest;
import org.jboss.tools.usage.ILoggingAdapter;
import org.jboss.tools.usage.IURLBuildingStrategy;
import org.jboss.tools.usage.Tracker;

/**
 * @author Andre Dietisheim
 */
public class UrlRevealingTracker extends Tracker {

	private String trackingUrl;
	private Lock lock;

	public UrlRevealingTracker(IURLBuildingStrategy urlBuildingStrategy, IHttpGetRequest httpGetRequest,
			ILoggingAdapter loggingAdapter) {
		super(urlBuildingStrategy, httpGetRequest, loggingAdapter);
		lock = new ReentrantLock();
	}

	@Override
	public void trackAsynchronously(FocusPoint focusPoint) {
		try {
			lock.lock();
			super.trackAsynchronously(focusPoint);
			lock.unlock();
		} catch (Exception e) {
			lock.unlock();
			throw new RuntimeException(e);
		}
	}

	@Override
	protected String getTrackingUrl(FocusPoint focusPoint) throws UnsupportedEncodingException {
		return trackingUrl = super.getTrackingUrl(focusPoint);
	}

	public String getTrackingUrl() {
		try {
			lock.lock();
			return trackingUrl;
		} finally {
			lock.unlock();
		}
	}
}