package org.jboss.tools.ui.bot.ext.config.requirement;

import static org.junit.Assert.fail;

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
		return SWTTestExt.configuredState.getDB().isConfigured
		&& SWTTestExt.configuredState.getDB().name.equals(TestConfigurator.currentConfig.getDB().name);			
	}

	@Override
	public void handle() {
		TestConfiguration configuration = TestConfigurator.currentConfig;
		
		// For internal hsqldb
		if (configuration.getDB().internal) {
			try {
				DatabaseHelper.addDriverIntoWorkspace();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
				Assert.fail("DB driver not found" + e);
			} catch (IOException e) {
				e.printStackTrace();
				Assert.fail("Unable to copy driver" + e);
			}
			DatabaseHelper.startHSQLDBServer("mydb", "xdb");
		}
		
		// Define Driver Entity and create driver
		DriverEntity entity = new DriverEntity();
		entity.setDrvPath(configuration.getDB().driverPath);
		entity.setJdbcString(configuration.getDB().jdbcString);
		entity.setProfileName(configuration.getDB().name);
		entity.setUser(configuration.getDB().username);
		entity.setPassword(configuration.getDB().password);
		entity.setDriverTemplateDescId(DatabaseHelper.getDriverTemplate(configuration.getDB().dbType));

		try {
			DatabaseHelper.createDriver(entity);
		} catch (ConnectionProfileException e) {
			log.error("Unable to create HSQL Driver" + e);
			Assert.fail("Unable to create HSQL Driver");			
		}
		
		if (!configuration.getDB().scriptPath.isEmpty()) {
			// Read script
			StringBuilder builder = readScript(configuration.getDB().scriptPath);
			
			// Open editor and run script
			DatabaseHelper.openSQLEditor(configuration.getDB().dbType, configuration.getDB().name.toString(), "Default" );
			DatabaseHelper.runSQLScript(builder.toString());		
		}
		
		log.info("DB Prepared");
		
		// Update Configured State
		SWTTestExt.configuredState.getDB().isConfigured=true;
		SWTTestExt.configuredState.getDB().name=TestConfigurator.currentConfig.getDB().name;
		SWTTestExt.configuredState.getDB().version=TestConfigurator.currentConfig.getDB().version;	
	}
	
	private StringBuilder readScript(String path) {
		File file = new File(path);
		StringBuilder builder = new StringBuilder();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new FileReader(file));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			Assert.fail("Unable to read script " + file.getAbsolutePath());
		}
		while(true) { 
			String line = null;
			try {
				line = reader.readLine();
				if (line == null) break;
			} catch (IOException e) {
				e.printStackTrace();
				fail("Can't read script" + file.getAbsolutePath());
			}
			builder.append(line);
			builder.append(System.getProperty("line.separator"));
		} 		
		return builder;
	}

}
