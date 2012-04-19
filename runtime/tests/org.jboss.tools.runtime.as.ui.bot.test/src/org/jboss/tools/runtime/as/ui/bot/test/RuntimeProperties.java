package org.jboss.tools.runtime.as.ui.bot.test;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

import org.jboss.tools.ui.bot.ext.SWTUtilExt;

/**
 * Reads the configured runtimes from the property file. 
 * 
 * @author Lucia Jelinkova
 *
 */
public class RuntimeProperties {

	private static final RuntimeProperties INSTANCE = new RuntimeProperties();
	
	private static Properties properties;
	
	private RuntimeProperties() {
		// singleton
	}
	
	public static RuntimeProperties getInstance() {
		return INSTANCE;
	}
	
	public String getRuntimePath(String runtimeId){
		if (properties == null){
			loadProperties();
		}
		
		String path = properties.getProperty(runtimeId);
		
		if (path == null){
			throw new IllegalArgumentException("No runtime path found for runtime id " + runtimeId);
		}
		
		return path;
	}

	private void loadProperties() {
		try {
			properties = new Properties();
			properties.load(new FileReader(SWTUtilExt.getResourceFile(Activator.PLUGIN_ID, "/runtimes.properties")));
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
