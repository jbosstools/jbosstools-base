package org.jboss.tools.ui.bot.ext.helper;

import java.io.File;
import java.io.IOException;
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
import org.jboss.tools.ui.bot.ext.Activator;
import org.jboss.tools.ui.bot.ext.types.DriverEntity;

public class DatabaseHelper {

		
	/**
	 * Create HSQLDB Driver 
	 * @throws ConnectionProfileException
	 * @return driver instance
	 */
	public static void createDriver(DriverEntity entity, String profileName) throws ConnectionProfileException {
		String driverPath;
		try {
			driverPath = new File(entity.getDrvPath()).getCanonicalPath(); //$NON-NLS-1$
		} catch (IOException e) {
			Activator.getDefault().getLog().log(new Status(IStatus.ERROR,
					Activator.PLUGIN_ID, "Can't create driver", e));
			return;
		}

		DriverInstance driver = DriverManager.getInstance().getDriverInstanceByName(entity.getInstanceName());
		if (driver == null) {
			TemplateDescriptor descr = TemplateDescriptor.getDriverTemplateDescriptor("org.eclipse.datatools.enablement.hsqldb.1_8.driver");
			IPropertySet instance = new PropertySetImpl(entity.getInstanceName(), "DriverDefn.Hypersonic DB");
			instance.setName(entity.getInstanceName());
			instance.setID("DriverDefn.Hypersonic DB");
			Properties props = new Properties();

			IConfigurationElement[] template = descr.getProperties();
			for (int i = 0; i < template.length; i++) {
				IConfigurationElement prop = template[i];
				String id = prop.getAttribute("id"); //$NON-NLS-1$

				String value = prop.getAttribute("value"); //$NON-NLS-1$
				props.setProperty(id, value == null ? "" : value); //$NON-NLS-1$
			}
			//props.setProperty("org.eclipse.datatools.connectivity.db.URL", "jdbc:hsqldb:file:testdb"); //$NON-NLS-1$
			props.setProperty("org.eclipse.datatools.connectivity.db.URL", entity.getJdbcString());
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_TYPE, descr.getId());
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_JARLIST, driverPath);

			instance.setBaseProperties(props);
			DriverManager.getInstance().removeDriverInstance(instance.getID());
			System.gc();
			DriverManager.getInstance().addDriverInstance(instance);
		}

		driver = DriverManager.getInstance().getDriverInstanceByName(entity.getInstanceName());
		if (driver != null && ProfileManager.getInstance().getProfileByName(profileName) == null) { //$NON-NLS-1$
			// create profile
			Properties props = new Properties();
			props.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, "DriverDefn.Hypersonic DB");
			props.setProperty(IDBConnectionProfileConstants.CONNECTION_PROPERTIES_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID,	driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_NAME_PROP_ID, entity.getDatabaseName()); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.PASSWORD_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBConnectionProfileConstants.SAVE_PASSWORD_PROP_ID, "false"); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.URL_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.URL_PROP_ID));

			ProfileManager.getInstance().createProfile(entity.getProfileName(),	entity.getProfileDescription(), IDBConnectionProfileConstants.CONNECTION_PROFILE_ID, props, "", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
	}
	
	/**
   * Create HSQLDB Driver and default connection profile 
   * @throws ConnectionProfileException
   * @return driver instance
   */
  public static void createDriver(DriverEntity entity) throws ConnectionProfileException {
    createDriver(entity,"DefaultDS");
  }
	
}
