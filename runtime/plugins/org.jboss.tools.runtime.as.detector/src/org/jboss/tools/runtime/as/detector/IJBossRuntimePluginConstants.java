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

public interface IJBossRuntimePluginConstants {
	public static final String DEFAULT_DS = "DefaultDS";
	public static final String RUNTIME = Messages.JBossRuntimeStartup_Runtime;
	public static final String EAP = "EAP"; //$NON-NLS-1$
	public static final String EAP_STD = "EAP_STD"; //$NON-NLS-1$
	public static final String SOA_P = "SOA-P"; //$NON-NLS-1$
	public static final String SOA_P_STD = "SOA-P-STD"; //$NON-NLS-1$
	public static final String EPP = "EPP"; //$NON-NLS-1$
	public static final String EWP = "EWP"; //$NON-NLS-1$
	public static final String AS = "AS"; //$NON-NLS-1$
	public static final String JBOSS_EAP_HOME = "../../../../jboss-eap/jboss-as"; 	// JBoss AS home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	public static final String JBOSS_EAP_HOME_CONFIGURATION = "../../jboss-eap/jboss-as"; 	// JBoss AS home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	public static final String SERVERS_FILE_NAME = "application_platforms.properties"; //$NON-NLS-1$
	public static final String SERVERS_FILE = "../../../../studio/" + SERVERS_FILE_NAME; //$NON-NLS-1$
	public static final String SERVERS_FILE_CONFIGURATION = "../../studio/" + SERVERS_FILE_NAME; //$NON-NLS-1$
	public static String RUNTIME_CONFIG_FORMAT_VERSION = "1.0"; //$NON-NLS-1$
	
	// This constants are made to avoid dependency with org.jboss.ide.eclipse.as.core plugin
	public static final String JBOSS_AS_RUNTIME_TYPE_ID[] = {
		"org.jboss.ide.eclipse.as.runtime.32", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.40", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.42", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.51", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.60", //$NON-NLS-1$		
		"org.jboss.ide.eclipse.as.runtime.eap.43", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.eap.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.70" //$NON-NLS-1$
		};
	
	public static final String HSQLDB_DRIVER_JAR_NAME = "hsqldb.jar"; //$NON-NLS-1$
	
	public static final String HSQLDB_DRIVER_3X_4X_LOCATION = "/server/default/lib/" + HSQLDB_DRIVER_JAR_NAME; //$NON-NLS-1$
	
	public static final String HSQLDB_DRIVER_5X_LOCATION = "/common/lib/" + HSQLDB_DRIVER_JAR_NAME; //$NON-NLS-1$
	
	// This constants are made to avoid dependency with org.jboss.ide.eclipse.as.core plugin
	public static final String JBOSS_AS_HSQL_DRIVER_LOCATION[] = {
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,		
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION
	};

	public static final String JBOSS_AS_TYPE_ID[] = {
		"org.jboss.ide.eclipse.as.32", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.40", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.42", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.51", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.60", //$NON-NLS-1$		
		"org.jboss.ide.eclipse.as.eap.43", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.eap.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.70" //$NON-NLS-1$
		};
	
	public static final String JBOSS_AS_NAME[] = {
		Messages.JBossRuntimeStartup_JBoss_Application_Server_3_2,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_4_0,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_4_2,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_5_0,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_5_1,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_6_0,
		Messages.JBossRuntimeStartup_JBoss_EAP_Server_4_3,
		Messages.JBossRuntimeStartup_JBoss_EAP_Server_5_0,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_7_0
		};
	
	public static final String JBOSS_AS_HOST = "localhost"; //$NON-NLS-1$

	public static final String JBOSS_AS_DEFAULT_CONFIGURATION_NAME = "default"; //$NON-NLS-1$

	//public static final String FIRST_START_PREFERENCE_NAME = "FIRST_START";

	public static final String HSQL_DRIVER_DEFINITION_ID 
												= "DriverDefn.Hypersonic DB"; //$NON-NLS-1$

	public static final String HSQL_DRIVER_NAME = "Hypersonic DB"; //$NON-NLS-1$

	public static final String HSQL_DRIVER_TEMPLATE_ID 
						= "org.eclipse.datatools.enablement.hsqldb.1_8.driver"; //$NON-NLS-1$

	public static final String DTP_DB_URL_PROPERTY_ID 
								= "org.eclipse.datatools.connectivity.db.URL"; //$NON-NLS-1$

	public static final String HSQL_PROFILE_ID = "org.eclipse.datatools.enablement.hsqldb.connectionProfile";

}
