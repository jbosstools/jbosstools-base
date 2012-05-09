/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Properties;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.datatools.connectivity.ConnectionProfileConstants;
import org.eclipse.datatools.connectivity.ConnectionProfileException;
import org.eclipse.datatools.connectivity.ProfileManager;
import org.eclipse.datatools.connectivity.db.generic.IDBConnectionProfileConstants;
import org.eclipse.datatools.connectivity.db.generic.IDBDriverDefinitionConstants;
import org.eclipse.datatools.connectivity.drivers.DriverInstance;
import org.eclipse.datatools.connectivity.drivers.DriverManager;
import org.eclipse.datatools.connectivity.drivers.IDriverMgmtConstants;
import org.eclipse.datatools.connectivity.drivers.IPropertySet;
import org.eclipse.datatools.connectivity.drivers.PropertySetImpl;
import org.eclipse.datatools.connectivity.drivers.models.TemplateDescriptor;
import org.eclipse.wst.server.core.IServerType;
import org.jboss.ide.eclipse.as.core.util.IJBossToolingConstants;
import org.jboss.tools.runtime.as.detector.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.as.detector.Messages;
import org.jboss.tools.runtime.as.detector.RuntimeAsActivator;

public class DriverUtility implements IJBossRuntimePluginConstants {
	public static final HashMap<String,String> SERVER_DRIVER_LOCATION = new HashMap<String, String>();
	static {
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_32, HSQLDB_DRIVER_3X_4X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_40, HSQLDB_DRIVER_3X_4X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_42, HSQLDB_DRIVER_3X_4X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_50, HSQLDB_DRIVER_5X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_51, HSQLDB_DRIVER_5X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_AS_60, HSQLDB_DRIVER_5X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_EAP_43, HSQLDB_DRIVER_3X_4X_LOCATION);
		SERVER_DRIVER_LOCATION.put(IJBossToolingConstants.SERVER_EAP_50, HSQLDB_DRIVER_5X_LOCATION);
	}

	/**
	 * Creates HSQL DB Driver
	 * @param jbossASLocation location of JBoss AS
	 * @param index 
	 * @throws ConnectionProfileException
	 * @return driver instance
	 */
	public void createDriver(String jbossASLocation, IServerType serverType) throws ConnectionProfileException {
		if(ProfileManager.getInstance().getProfileByName(DEFAULT_DS) != null) {
			// Don't create the driver a few times
			return;
		}
		String driverPath;
		try {
			driverPath = getDriverPath(jbossASLocation, serverType);
		} catch (IOException e) {
			RuntimeAsActivator.getDefault().getLog().log(new Status(IStatus.ERROR,
					RuntimeAsActivator.PLUGIN_ID, Messages.JBossRuntimeStartup_Cannott_create_new_HSQL_DB_Driver, e));
			return;
		}
		if (driverPath == null) {
			RuntimeAsActivator.getDefault().getLog().log(new Status(IStatus.ERROR,
					RuntimeAsActivator.PLUGIN_ID, Messages.JBossRuntimeStartup_Cannot_create_new_DB_Driver));
		}

		DriverInstance driver = getDriver(serverType);
		if (driver == null) {
			TemplateDescriptor descr = getDriverTemplateDescriptor(serverType);
			IPropertySet instance = new PropertySetImpl(getDriverName(serverType), getDriverDefinitionId(serverType));
			instance.setName(getDriverName(serverType));
			instance.setID(getDriverDefinitionId(serverType));
			Properties props = new Properties();

			IConfigurationElement[] template = descr.getProperties();
			for (int i = 0; i < template.length; i++) {
				IConfigurationElement prop = template[i];
				String id = prop.getAttribute("id"); //$NON-NLS-1$

				String value = prop.getAttribute("value"); //$NON-NLS-1$
				props.setProperty(id, value == null ? "" : value); //$NON-NLS-1$
			}
			props.setProperty(DTP_DB_URL_PROPERTY_ID, getDriverUrl(serverType)); //$NON-NLS-1$
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_TYPE, descr.getId());
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_JARLIST, driverPath);
			if( isAS7StyleServer(serverType)) {
				props.setProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID, "org.h2.Driver");
				props.setProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID, "H2 driver");
				props.setProperty(IDBDriverDefinitionConstants.URL_PROP_ID, "jdbc:h2:mem");
				props.setProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID, "sa");
			}
			
			instance.setBaseProperties(props);
			DriverManager.getInstance().removeDriverInstance(instance.getID());
			System.gc();
			DriverManager.getInstance().addDriverInstance(instance);
		}

		driver = DriverManager.getInstance().getDriverInstanceByName(getDriverName(serverType));
		if (driver != null && ProfileManager.getInstance().getProfileByName(DEFAULT_DS) == null) { //$NON-NLS-1$
			// create profile
			Properties props = new Properties();
			props.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, 
					getDriverDefinitionId(serverType));
			props.setProperty(IDBConnectionProfileConstants.CONNECTION_PROPERTIES_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID,	driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_NAME_PROP_ID, "Default"); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.PASSWORD_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBConnectionProfileConstants.SAVE_PASSWORD_PROP_ID, "false"); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.URL_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.URL_PROP_ID));

			if( isAS7StyleServer(serverType)) {
				ProfileManager.getInstance().createProfile(DEFAULT_DS,	Messages.JBossRuntimeStartup_The_JBoss_AS_H2_embedded_database, H2_PROFILE_ID, props, "", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			} else {
				ProfileManager.getInstance().createProfile(DEFAULT_DS,	Messages.JBossRuntimeStartup_The_JBoss_AS_Hypersonic_embedded_database, HSQL_PROFILE_ID, props, "", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}
		
	}

	protected static String getDriverUrl(IServerType serverType) {
		if( isAS7StyleServer(serverType)) {
			return "jdbc:h2:mem";
		} else {
			return "jdbc:hsqldb:.";
		}
	}

	private static String getDriverDefinitionId(IServerType serverType) {
		if( isAS7StyleServer(serverType)) {
			return H2_DRIVER_DEFINITION_ID;
		} else {
			return HSQL_DRIVER_DEFINITION_ID;
		}
	}

	private static String getDriverName(IServerType serverType) {
		if( isAS7StyleServer(serverType)) {
			return H2_DRIVER_NAME;
		} else {
			return HSQL_DRIVER_NAME;
		}
	}

	protected static TemplateDescriptor getDriverTemplateDescriptor(IServerType serverType) {
		if( isAS7StyleServer(serverType)) {
			return TemplateDescriptor.getDriverTemplateDescriptor(H2_DRIVER_TEMPLATE_ID);
		} else {
			return TemplateDescriptor.getDriverTemplateDescriptor(HSQL_DRIVER_TEMPLATE_ID);
		}
	}

	protected static DriverInstance getDriver(IServerType serverType) {
		if( isAS7StyleServer(serverType)) {
			return DriverManager.getInstance().getDriverInstanceByName(H2_DRIVER_NAME);
		}
		return DriverManager.getInstance().getDriverInstanceByName(HSQL_DRIVER_NAME);
	}

	private static String getDriverPath(String jbossASLocation, IServerType serverType)
			throws IOException {
		String driverPath;
		if (isAS7StyleServer(serverType)) {
			File file = new File(jbossASLocation + "/modules/com/h2database/h2/main").getCanonicalFile();
			File[] fileList = file.listFiles(new FilenameFilter() {
				
				@Override
				public boolean accept(File dir, String name) {
					if (name.startsWith("h2") && name.endsWith(".jar")) {
						return true;
					}
					return false;
				}
			});
			if (fileList != null && fileList.length > 0) {
				return fileList[0].getCanonicalPath();
			}
			return null;
		} else {
			String loc = SERVER_DRIVER_LOCATION.get(serverType.getId());
			driverPath = new File(jbossASLocation + loc).getCanonicalPath(); //$NON-NLS-1$
		}
		return driverPath;
	}

	private static boolean isAS7StyleServer(IServerType type) {
		if( type != null ) {
			String id = type.getId();
			return IJBossToolingConstants.SERVER_AS_70.equals(id) || 
					IJBossToolingConstants.SERVER_AS_71.equals(id) ||
					IJBossToolingConstants.SERVER_EAP_60.equals(id);
		}
		return false;
	}

}
