/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.usage.internal;

import org.jboss.tools.foundation.core.usage.IUsageTracker;
import org.osgi.framework.BundleContext;
import org.osgi.util.tracker.ServiceTracker;

public class UsageTrackerService extends ServiceTracker implements IUsageTracker {
	
	private IUsageTracker defaultTracker;

	public UsageTrackerService(BundleContext context) {
		super(context, IUsageTracker.class.getName(), null);
		this.defaultTracker = new DefaultUsageTracker();
	}
	
	private IUsageTracker findUsageService(){
		Object[] services = getServices();
		
		if(services != null){
			for(Object service : services){
				if(service.getClass().getName().equals("org.jboss.tools.usage.tracker.internal.UsageTracker")){
					return (IUsageTracker)service;
				}
			}
		}
		
		return defaultTracker;
	}
	
	public void sendDailyEvent(String eventCategory, String eventAction, String eventLabel) {
		IUsageTracker service = findUsageService();
		
		service.sendDailyEvent(eventCategory, eventAction, eventLabel);
	}

	public void sendLiveEvent(String eventCategory, String eventAction, String eventLabel) {
		IUsageTracker service = findUsageService();
		
		service.sendLiveEvent(eventCategory, eventAction, eventLabel);
	}

}
