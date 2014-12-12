/*******************************************************************************
 * Copyright (c) 2014 Zend Technologies Ltd.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Zend Technologies Ltd. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.event;

import org.jboss.tools.usage.googleanalytics.RequestType;

/**
 * @author Kaloyan Raev
 */
public interface IUsageReporterService {

	/**
	 * Registers the event type
	 * 
	 * @param type
	 */
	public void registerEvent(UsageEventType type);

	/**
	 * Tracks a user's event
	 * 
	 * @param event
	 */
	public void trackEvent(UsageEvent event);

	/**
	 * Tracks a user's event
	 * 
	 * @param pagePath
	 * @param title
	 * @param event
	 * @param type
	 * @param startNewVisitSession
	 */
	public void trackEvent(String pagePath, String title, UsageEvent event, RequestType type,
			boolean startNewVisitSession);
	
	/**
	 * Doesn't send a tracking request instantly but remembers the event's value for tracking events once a day.
	 * If the type of this event was used for sending or counting events a day before then a new event with a sum (if bigger than 0) of all previously collected events is sent.
	 * Category, action names and labels are taken into account when values are being counted.
	 * For events without labels and/or values the "N/A" is used as a label and "1" is used as the default value.
	 * @param event  
	 */
	public void countEvent(UsageEvent event);
	
	/**
	 * Sends a tracking request for all daily events if it's time to send them 
	 */
	public void trackCountEvents();

}
