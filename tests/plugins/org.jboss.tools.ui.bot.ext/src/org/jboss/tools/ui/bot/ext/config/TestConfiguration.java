package org.jboss.tools.ui.bot.ext.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.Activator;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator.Keys;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator.Values;

public class TestConfiguration {
	private static final Logger log = Logger.getLogger(TestConfiguration.class);

	private Properties swtTestProperties = new Properties();

	public String getProperty(String key) {
		return swtTestProperties.getProperty(key);
	}

	private final String propName;
	private final String propFile;

	private ServerBean server;
	private SeamBean seam;
	private ESBBean esb;
	private JavaBean java;

	public TestConfiguration(String propName, String propFile) throws Exception {
		this.propName = propName;
		this.propFile = propFile;
		if (!"".equals(propFile)) {
			if (new File(propFile).exists()) {
				log.info("Loading configuration file '" + propFile + "'");
				swtTestProperties.load(new FileInputStream(propFile));
			} else {
				throw new IOException(propName + " " + propFile + " does not exist!");
			}

		} else {
			log.info("Loading default configuration");
			swtTestProperties.load(new FileInputStream(SWTTestExt.util
					.getResourceFile(Activator.PLUGIN_ID,
							"/SWTBotTest-default.properties")));
		}
		// properties got loaded
		java = JavaBean.fromString(getProperty(Keys.JAVA));
		printConfig(Keys.JAVA, java);
		server = ServerBean.fromString(getProperty(Keys.SERVER));
		printConfig(Keys.SERVER, server);
		seam = SeamBean.fromString(getProperty(Keys.SEAM));
		printConfig(Keys.SEAM, seam);
		esb = ESBBean.fromString(getProperty(Keys.ESB));
		printConfig(Keys.ESB, esb);
		
		checkConfig();
	}

	private static void printConfig(String propName, Object bean) {
		if (bean == null) {
			log.info("Property " + propName + " not found, " + propName
					+ " not configured");
		} else {
			log.info("Configured " + bean.toString());
		}
	}

	private boolean checkConfig() throws Exception {
			if (java != null)
				checkDirExists(java.javaHome);
			if (seam != null)
				checkDirExists(seam.seamHome);
			if (server != null)
				checkDirExists(server.runtimeHome);
			if (esb != null)
				checkDirExists(esb.esbHome);
			// special checks capturing dependency of server on java
			if (java == null
					&& server != null
					&& !server.withJavaVersion
							.equals(Values.SERVER_WITH_DEFAULT_JAVA)) {
				throw new Exception(
						"Server is configured to run with java version="
								+ server.withJavaVersion
								+ " but no JAVA is configured");
			}
			if (java != null && server!=null) {
				if (!java.version.equals(server.withJavaVersion)
						&& !Values.SERVER_WITH_DEFAULT_JAVA
								.equals(server.withJavaVersion)) {
					throw new Exception(
							"Server is configured to run with java version="
									+ server.withJavaVersion
									+ " but JAVA is configured with "
									+ java.version);
				}
			}
			return true;

	}

	private static void checkDirExists(String dir) throws FileNotFoundException {
		if (!new File(dir).exists() || !new File(dir).isDirectory()) {
			throw new FileNotFoundException("File '" + dir
					+ "' does not exist or is not directory");
		}
	}

	public ESBBean getEsb() {
		return esb;
	}

	public SeamBean getSeam() {
		return seam;
	}

	public ServerBean getServer() {
		return server;
	}

	public JavaBean getJava() {
		return java;
	}

	public String getPropFile() {
		return propFile;
	}

	public String getPropName() {
		return propName;
	}
}
