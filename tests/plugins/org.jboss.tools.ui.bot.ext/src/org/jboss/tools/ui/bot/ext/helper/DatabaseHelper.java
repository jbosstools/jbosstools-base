package org.jboss.tools.ui.bot.ext.helper;

import static org.eclipse.swtbot.eclipse.finder.matchers.WithPartName.withPartName;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
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
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.eclipse.ui.IViewReference;
import org.hamcrest.Matcher;
import org.hsqldb.Server;
import org.jboss.tools.ui.bot.ext.Activator;
import org.jboss.tools.ui.bot.ext.SWTEclipseExt;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.types.DriverEntity;
import org.jboss.tools.ui.bot.ext.types.PerspectiveType;
import org.jboss.tools.ui.bot.ext.types.ViewType;

public class DatabaseHelper {
	
	public static int SLEEP = 1000;
	public static Logger log = Logger.getLogger(DatabaseHelper.class);
	private static boolean hsqlRunning = false;

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
					Activator.PLUGIN_ID, "Can't get driver' path", e));
			return;
		}

		DriverInstance driver = DriverManager.getInstance().getDriverInstanceByName(entity.getInstanceName());
		if (driver == null) {
			TemplateDescriptor descr = TemplateDescriptor.getDriverTemplateDescriptor(entity.getDriverTemplateDescId());
			IPropertySet instance = new PropertySetImpl(entity.getInstanceName(), entity.getDriverDefId());
			instance.setName(entity.getInstanceName());
			instance.setID(entity.getDriverDefId());
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
			props.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, entity.getDriverDefId());
			props.setProperty(IDBConnectionProfileConstants.CONNECTION_PROPERTIES_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID,	driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_NAME_PROP_ID, entity.getDatabaseName()); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.PASSWORD_PROP_ID, entity.getPassword()); //$NON-NLS-1$ //my
			props.setProperty(IDBConnectionProfileConstants.SAVE_PASSWORD_PROP_ID, "false"); //$NON-NLS-1$ 
			props.setProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID, entity.getUser()/*driver.getProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID)*/);
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
	/**
   * 
   */
	public static void openSQLEditor(DBType type, String profile, String db) {
		SWTEclipseExt eclipse = new SWTEclipseExt();
		SWTWorkbenchBot bot = eclipse.getBot();

		eclipse.openPerspective(PerspectiveType.DB_DEVELOPMENT);
		eclipse.showView(ViewType.DATA_SOURCE_EXPLORER);

		bot.saveAllEditors();
		bot.closeAllEditors();

		bot.sleep(SLEEP);

		Matcher<IViewReference> matcher = withPartName("Data Source Explorer");
		SWTBotView view = bot.view(matcher);

		bot.sleep(SLEEP);
		SWTBotTree tree = view.bot().tree();

		// Open SQL Scrapbook
		SWTBotTreeItem item = tree.expandNode("Database Connections")
				.expandNode(profile);
		item.contextMenu("Connect").click();
		item.contextMenu("Open SQL Scrapbook").click();

		// Set SQL Scrapbook
		SWTBotEditor editor = bot.editorByTitle("SQL Scrapbook 0");
		editor.setFocus();
		bot.comboBoxWithLabelInGroup("Type:", "Connection profile")
				.setSelection(getTypeCombo(type));
		bot.comboBoxWithLabelInGroup("Name:", "Connection profile")
				.setSelection(profile);
		bot.comboBoxWithLabelInGroup("Database:", "Connection profile")
				.setSelection(db);
	}

	/**
	 * 
	 * @param script
	 */
	public static void runSQLScript(String script) {
		SWTEclipseExt eclipse = new SWTEclipseExt();
		SWTWorkbenchBot bot = eclipse.getBot();

		SWTBotEditor editor = bot.editorByTitle("SQL Scrapbook 0");
		editor.toTextEditor().setText(script);

		// Execute Script and close
		editor.toTextEditor().contextMenu("Execute All").click();
		editor.toTextEditor().setText("");		
		editor.close();
		bot.sleep(SLEEP);
	}

	/**
	 * Database type enum
	 * 
	 * @author jpeterka
	 * 
	 */
	public enum DBType {
		hsqldb18, db2_97, mssql2005, mssql2008, mysql50, mysql51, oracle10g, oracle11gR1, oracle11gR1RAC, oracle11gR2, oracle11gR2RAC, postgresql82, postgresql83, postgresql84, sybase15, teiid
	}
	
	/**
	 * Return driver template for creating new connection
	 * 
	 * @param type
	 * @return
	 */
	public static String getDriverTemplate(DBType type) {
		String ret = "";
		switch (type) {
		case hsqldb18:
			ret = "org.eclipse.datatools.enablement.hsqldb.1_8.driver";
			break;
		case db2_97:
			ret = "org.eclipse.datatools.enablement.ibm.db2.luw.driverTemplate";
			break;
		case mssql2005:
			ret = "org.eclipse.datatools.enablement.msft.sqlserver.2005.driverTemplate";
			break;
		case mssql2008:
			ret = "org.eclipse.datatools.enablement.msft.sqlserver.2008.driverTemplate";
			break;
		case mysql50:
			ret = "org.eclipse.datatools.enablement.mysql.5_0.driverTemplate";
			break;
		case mysql51:
			ret = "org.eclipse.datatools.enablement.mysql.5_1.driverTemplate";
			break;
		case oracle10g:
			ret = "org.eclipse.datatools.enablement.oracle.10.driverTemplate";
			break;
		case oracle11gR1: // Intentionally empty
		case oracle11gR1RAC: // Intentionally empty
		case oracle11gR2: // Intentionally empty
		case oracle11gR2RAC:
			ret = "org.eclipse.datatools.enablement.oracle.11.driverTemplate";
			break;
		case postgresql82: // Intentionally empty
		case postgresql83: // Intentionally empty
		case postgresql84: // Intentionally empty
		case sybase15:
			ret = "org.eclipse.datatools.connectivity.db.sybase.ase.genericDriverTemplate_15";
			break;
		case teiid:
			ret = "org.teiid.datatools.connectivity.driver.serverDriverTemplate";
			break;
		default:
			fail("Unknown db type");
			break;
		}
		return ret;
	}

	/**
	 * Resolves db type for eclipse usage in SQL editor
	 * 
	 * @param type
	 * @return
	 */
	public static String getTypeCombo(DBType type) {
		String ret = "";
		switch (type) {
		case hsqldb18:
			ret = "HSQLDB_1.8";
			break;
		case db2_97:
			ret = "DB2 UDB_V9.1";
			break;
		case mssql2005:
			ret = "SQL Server_2005";
			break;
		case mssql2008:
			ret = "SQL Server_2008";
			break;
		case mysql50:
			ret = "MySQL_5.0";
			break;
		case mysql51:
			ret = "MySQL_5.1";
			break;
		case oracle10g:
			ret = "Oracle_10";
			break;
		case oracle11gR1: // Intentionally empty
		case oracle11gR1RAC: // Intentionally empty
		case oracle11gR2: // Intentionally empty
		case oracle11gR2RAC:
			ret = "Oracle_11";
			break;
		case postgresql82: // Intentionally empty
		case postgresql83: // Intentionally empty
		case postgresql84:
			ret = "postgres_8.x";
			break;
		case sybase15:
			ret = "Sybase_ASE_15.x";
			break;
		default:
			fail("Unknown db type");
			break;
		}
		return ret;
	}
	
	/**
	 * Resolves db type for eclipse usage in SQL editor
	 * 
	 * @param type
	 * @return
	 */
	public static String getDialect(DBType type) {
		// TODO verify values
		String ret = "";
		switch (type) {
		case hsqldb18:
			ret = "HSQL";
			break;
		case db2_97:
			ret = "DB2 UDB_V9.1";
			break;
		case mssql2005:
			ret = "SQL Server_2005";
			break;
		case mssql2008:
			ret = "SQL Server_2008";
			break;
		case mysql50:
			ret = "MySQL_5.0";
			break;
		case mysql51:
			ret = "MySQL 5";
			break;
		case oracle10g:
			ret = "Oracle_10";
			break;
		case oracle11gR1: // Intentionally empty
		case oracle11gR1RAC: // Intentionally empty
		case oracle11gR2: // Intentionally empty
		case oracle11gR2RAC:
			ret = "Oracle_11";
			break;
		case postgresql82: // Intentionally empty
		case postgresql83: // Intentionally empty
		case postgresql84:
			ret = "PostgreSQL";
			break;
		case sybase15:
			ret = "Sybase_ASE_15.x";
			break;
		default:
			fail("Unknown db type");
			break;
		}
		return ret;
	}
	
	
	/**
	 * Run HSQLDB database in server mode
	 * @param file
	 * @param dbname
	 */
	public static void startHSQLDBServer(final String file, final String dbname) {
		Thread hsqlThread = null;
		log.info("Starting HSQLDB...");
		try {
			Class.forName("org.hsqldb.jdbcDriver");
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Runnable runable = new Runnable() {
			
			public void run() {
				Server.main(new String[] {"-database.0","file:" + file,"-dbname.0",dbname });
			}
		};
		
		hsqlThread = new Thread(runable);
		hsqlThread.start();
		hsqlRunning = true;
		log.info("HSQLDB started");
	}
	
	/**
	 * Stop HSQL Database by sending SHUTDOWN command
	 */
	public static void stopHSQLDBServer() {
		if (!hsqlRunning) return;
		
		try {		
			Class.forName("org.hsqldb.jdbcDriver");
			
			Connection connection = java.sql.DriverManager.getConnection("jdbc:hsqldb:hsql://localhost/xdb");
			
			Statement statement = connection.createStatement();
			ResultSet resultset = statement.executeQuery("SHUTDOWN");
			
			resultset.close();
			statement.close();
			connection.close();

			hsqlRunning = false;
			log.error("Internal hql server stopped");

			
		} catch (SQLException e) {
			
		}
		catch (ClassNotFoundException e) {
			log.error("Unable to stop HSQLDB " + e);
		}
	}
	
	/**
	 * Returns flag refering if HSQLDB is running
	 */
	public static boolean isHSQLDBRunning() {
		return hsqlRunning;
	}
	
	/**
	 * Add HSQLDB driver into project
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public static void addDriverIntoWorkspace() throws FileNotFoundException, IOException {
		File in = SWTUtilExt.getResourceFile(Activator.PLUGIN_ID, "drv","hsqldb.jar");
		File out = new File(Platform.getLocation() + File.separator + "hsqldb.jar");
		
        FileChannel inChannel = null;
        FileChannel outChannel = null;

		inChannel = new FileInputStream(in).getChannel();
		outChannel = new FileOutputStream(out).getChannel();

    	inChannel.transferTo(0, inChannel.size(),	outChannel);

    	if (inChannel != null) inChannel.close();
    	if (outChannel != null) outChannel.close();
    	log.info("Driver hsqldb.jar copied");
	}

	public static String getDriverClass(DBType type) {
		// TODO verify values
		String ret = "";
		switch (type) {
		case hsqldb18:
			ret = "org.hsqldb.jdbcDriver";
			break;
		case db2_97:
			ret = "DB2 UDB_V9.1";
			break;
		case mssql2005:
			ret = "SQL Server_2005";
			break;
		case mssql2008:
			ret = "SQL Server_2008";
			break;
		case mysql50:
			ret = "com.mysql.jdbc.Driver";
			break;
		case mysql51:
			ret = "com.mysql.jdbc.Driver";
			break;
		case oracle10g:
			ret = "Oracle_10";
			break;
		case oracle11gR1: // Intentionally empty
		case oracle11gR1RAC: // Intentionally empty
		case oracle11gR2: // Intentionally empty
		case oracle11gR2RAC:
			ret = "Oracle_11";
			break;
		case postgresql82: // Intentionally empty
		case postgresql83: // Intentionally empty
		case postgresql84:
			ret = "org.postgresql.Driver";
			break;
		case sybase15:
			ret = "Sybase_ASE_15.x";
			break;
		default:
			fail("Unknown db type");
			break;
		}
		return ret;
	}
}
