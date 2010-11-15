package org.jboss.tools.ui.bot.ext.config;

import static org.junit.Assert.fail;

import java.io.File;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.ui.bot.ext.helper.DatabaseHelper.DBType;

/**
 * DB Bean class representing property string data
 * @author jpeterka
 *
 */
public class DBBean {
	public String version;
	public String jdbcString;
	public String username;
	public String password;
	public String driverPath;
	public String scriptPath;
	public DBType dbType; 
	public String name;
	public boolean internal = false;
	
	public static DBBean fromString(String propValue) throws Exception{
		try {
			if (propValue==null) {
				return null;
			}
			String[] dbParams = propValue.split(",");
			DBBean bean = new DBBean();
			bean.dbType=getDBType(dbParams[0]);
			bean.version = dbParams[1];
			bean.driverPath=dbParams[2];
			bean.jdbcString=dbParams[3];
			bean.username=dbParams[4];
			// optional
			try {
				bean.password=dbParams[5];			
			} catch (Exception ex) {
				bean.password = "";
			}
			try {
				bean.scriptPath=dbParams[6];			
			} catch (Exception ex) {
				bean.scriptPath= "";
			}				
			bean.name=bean.dbType.toString() + "_" + bean.version;

			// Internal version
			if (bean.version.equalsIgnoreCase("internal")) {
				bean.internal = true;
				bean.version="1.8";
				bean.internal = true;
				bean.driverPath = Platform.getLocation() + File.separator + "hsqldb.jar";
				bean.jdbcString = "jdbc:hsqldb:hsql://localhost/xdb";
				bean.username = "sa";
				bean.password = "";						
			} else {
				bean.internal = false;
			}

			return bean;
			}
			catch (Exception ex) {
				throw new Exception("Cannot parse DB property line",ex);
			}
	}
	@Override
	public String toString() {
		return String.format("DB runtime version=%s, jdbc_string=%s, driver_path=%s ",
				this.version, this.jdbcString, this.driverPath);
	}
	
	/**
	 * A common method for all enums since they can't have another base class
	 * @param <T> Enum type
	 * @param c enum type. All enums must be all caps.
	 * @param string case insensitive
	 * @return corresponding enum, or null
	 */
	public static <T extends Enum<T>> T getEnumFromString(Class<T> c, String string)
	{
	    if( c != null && string != null )
	    {
	        try
	        {
	            return Enum.valueOf(c, string.trim());
	            //return Enum.valueOf(c, string.trim().toUpperCase());
	        }
	        catch(IllegalArgumentException ex)
	        {
	        }
	    }
	    return null;
	}
	/**
	 * Validate given db type, fix lower/upper case if needed 
	 */	
	private static DBType getDBType(String type) {		
		// list of supported database types (except hsql)
		String[] valid = {"hsqldb18","db2_97","mssql2005","mssql2008","mysql50","mysql51","oracle10g",
				"oracle11gR1","oracle11gR1RAC","oracle11gR2","oracle11gR2RAC","postgressql82","postgresql83",
				"postgresql84","sybase15"};		

		for (String v : valid) {
			if (v.equalsIgnoreCase(type)) {
				return getEnumFromString(DBType.class, v);
			}
		}

		fail("Database type is not valid");
		throw new IllegalArgumentException("Can't get DB type");
	}
	

}