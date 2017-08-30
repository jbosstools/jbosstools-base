/*******************************************************************************
 * Copyright (c) 2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.event;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.googleanalytics.RequestType;
import org.jboss.tools.usage.internal.environment.eclipse.IJBossToolsEclipseEnvironment;

/**
 * Analytics specific provider interface for sending usage requests.
 */
@FunctionalInterface
public interface EventSender {
	boolean sendRequest(IJBossToolsEclipseEnvironment environment, String pagePath, String title,
			UsageEvent event,
			RequestType type,
			boolean startNewVisitSession);
}
