package org.jboss.tools.ui.bot.ext.config.requirement;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.eclipse.datatools.connectivity.ConnectionProfileException;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfiguration;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
import org.jboss.tools.ui.bot.ext.helper.DatabaseHelper;
import org.jboss.tools.ui.bot.ext.types.DriverEntity;
import org.junit.Assert;

public class PrepareDB extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getDB().isConfigured;
	}

	@Override
	public void handle() {
		TestConfiguration configuration = TestConfigurator.currentConfig;
		
		// Define Driver Entity
		DriverEntity entity = new DriverEntity();
		entity.setDrvPath(configuration.getDB().driverPath);
		entity.setJdbcString(configuration.getDB().jdbcString);
		entity.setProfileName(configuration.getDB().dbType.toString());
		entity.setUser("sa");
		entity.setPassword("");
		entity.setDriverTemplateDescId(DatabaseHelper.getDriverTemplate(configuration.getDB().dbType));

		try {
			DatabaseHelper.createDriver(entity);
		} catch (ConnectionProfileException e) {
			log.error("Unable to create HSQL Driver" + e);
			Assert.fail();			
		}
		
		File file = new File(configuration.getDB().scriptPath);
		StringBuilder builder = new StringBuilder();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(file));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		while(true) { 
			String line = null;
			try {
				line = reader.readLine();
				if (line == null) break;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			builder.append(line);
		} 
		DatabaseHelper.openSQLEditor(configuration.getDB().dbType, configuration.getDB().dbType.toString(), "Default" );
		DatabaseHelper.runSQLScript(builder.toString());
		
		if (configuration.getDB().dbType.equals("HSQLDB_1.8")) {
			// TODO
		}
	}

}
