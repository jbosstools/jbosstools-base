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
package org.jboss.tools.runtime.as.detector;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.runtime.as.detector.messages"; //$NON-NLS-1$
	public static String JBossRuntimeStartup_JBoss_Application_Server_6_0;
	public static String JBossRuntimeStartup_JBoss_Application_Server_7_0;
	public static String JBossRuntimeStartup_JBoss_Application_Server_7_1;
	public static String JBossRuntimeStartup_Cannot_create_new_JBoss_Server;
	public static String JBossRuntimeStartup_Cannott_create_new_DTP_Connection_Profile;
	public static String JBossRuntimeStartup_Cannott_create_new_HSQL_DB_Driver;
	public static String JBossRuntimeStartup_Cannot_create_new_DB_Driver;
	public static String JBossRuntimeStartup_JBoss_Application_Server_3_2;
	public static String JBossRuntimeStartup_JBoss_Application_Server_4_0;
	public static String JBossRuntimeStartup_JBoss_Application_Server_4_2;
	public static String JBossRuntimeStartup_JBoss_Application_Server_5_0;
	public static String JBossRuntimeStartup_JBoss_Application_Server_5_1;
	public static String JBossRuntimeStartup_JBoss_EAP_Server_4_3;
	public static String JBossRuntimeStartup_JBoss_EAP_Server_5_0;
	public static String JBossRuntimeStartup_JBoss_EAP_Server_6_0;
	// NEW_SERVER_ADAPTER add logic for new adapter here
	public static String JBossRuntimeStartup_Runtime;
	public static String JBossRuntimeStartup_The_JBoss_AS_Hypersonic_embedded_database;
	public static String JBossRuntimeStartup_The_JBoss_AS_H2_embedded_database;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
