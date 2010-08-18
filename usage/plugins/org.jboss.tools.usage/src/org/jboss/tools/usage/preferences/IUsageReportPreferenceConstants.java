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
package org.jboss.tools.usage.preferences;

/**
 * @author Andre Dietisheim
 */
public interface IUsageReportPreferenceConstants {

	/**
	 * Identifies the preferences that states whether the user allows us to
	 * report usage.
	 */
	public static final String USAGEREPORT_ENABLED = "allowUsageReportPreference";

	/**
	 * Identifies the preferences that states whether the user allows us to
	 * report usage.
	 */
	public static final String ASK_USER = "askUserPreference";

	public static final String ECLIPSE_INSTANCE_ID = "eclipseInstanceId";
}
