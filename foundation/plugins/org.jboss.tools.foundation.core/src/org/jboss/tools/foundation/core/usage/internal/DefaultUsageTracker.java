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

public class DefaultUsageTracker implements IUsageTracker {

	public DefaultUsageTracker() {
	}

	@Override
	public void sendDailyEvent(String eventCategory, String eventAction, String eventLabel) {
	}

	@Override
	public void sendLiveEvent(String eventCategory, String eventAction, String eventLabel) {
	}

}
