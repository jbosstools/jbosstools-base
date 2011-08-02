package org.jboss.tools.ui.bot.ext.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.Activator;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator.Keys;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator.Values;

/**
 * This class represents 1 test configuration. Every instance is filled by 1
 * properties file which defines : which server/seam/esb/bpm .. runtimes will be
 * present etc
 * 
 * @author lzoubek@redhat.com
 */
public class TestConfiguration {
	private static final Logger log = Logger.getLogger(TestConfiguration.class);
	private static final Pattern propPattern = Pattern
			.compile("\\$\\{([^\\}]+)\\}");
	private Properties configProperties = new Properties();

	public String getProperty(String key) {
		return configProperties.getProperty(key);
	}

	private final String propName;
	private final String propFile;

	private ServerBean server;
	private SeamBean seam;
	private ESBBean esb;
	private JavaBean java;
	private JBPMBean jbpm;
	private DBBean db;
	private RemoteSystemBean remoteSystem;
	private SecureStorage secureStorage;

	public TestConfiguration(String propName, String propFile) throws Exception {
		this.propName = propName;
		this.propFile = propFile;
		if (!"".equals(propFile)) {
			if (new File(propFile).exists()) {
				log.info("Loading configuration file '" + propFile + "'");
				configProperties.load(new FileInputStream(propFile));
			} else {
				throw new IOException(propName + " " + propFile
						+ " does not exist!");
			}

		} else {
			log.info("Loading default configuration");
			configProperties.load(new FileInputStream(SWTUtilExt
					.getResourceFile(Activator.PLUGIN_ID,
							"/SWTBotTest-default.properties")));
		}
		// properties got loaded
		substSystemProperties();
		java = JavaBean.fromString(getProperty(Keys.JAVA));
		printConfig(Keys.JAVA, java);
		server = ServerBean.fromString(getProperty(Keys.SERVER),
				getProperty(Keys.SERVER + TestConfigurator.RUNTIME_URL_SUFFIX));
		printConfig(Keys.SERVER, server);
		remoteSystem = RemoteSystemBean.fromString(getProperty(Keys.RS));
		printConfig(Keys.RS, remoteSystem);
		seam = SeamBean.fromString(getProperty(Keys.SEAM));
		printConfig(Keys.SEAM, seam);
		esb = ESBBean.fromString(getProperty(Keys.ESB));
		printConfig(Keys.ESB, esb);
		jbpm = JBPMBean.fromString(getProperty(Keys.JBPM));
		printConfig(Keys.JBPM, jbpm);
		db = DBBean.fromString(getProperty(Keys.DB));
		printConfig(Keys.DB, db);
		secureStorage = SecureStorage.fromString(Keys.SS, getProperty(Keys.SS));
		printConfig("Secure Storage", secureStorage);
		checkConfig();
	}

	/**
	 * replaces system properties in configuration property values
	 */
	private void substSystemProperties() {
		Matcher m;
		for (Object key : configProperties.keySet()) {
			String val = configProperties.getProperty(key.toString());
			if (val != null) {
				m = propPattern.matcher(val);
				while (m.find()) {
					val = val.replaceFirst(
							propPattern.toString(),
							Matcher.quoteReplacement(System.getProperty(
									m.group(1), "${" + m.group(1) + "}")));
				}
				configProperties.put(key, val);
			}
		}
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
			checkDirExists(java.runtimeHome);
		if (seam != null)
			checkDirExists(seam.runtimeHome);
		if (server != null)
			checkDirExists(server.runtimeHome);
		if (esb != null)
			checkDirExists(esb.runtimeHome);
		if (jbpm != null)
			checkDirExists(jbpm.runtimeHome);
		if (db != null) {
			if (!db.internal)
				checkFileExists(db.driverPath);
			if (!db.scriptPath.isEmpty())
				checkFileExists(db.scriptPath);
		}
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
		if (java != null && server != null) {
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

		if (server != null) {
			if (server.remoteSystem != null) {
				if (remoteSystem != null) {
					if (!server.remoteSystem.equals(remoteSystem.host)) {
						throw new Exception(
								"Server is configured with remote system called "
										+ server.remoteSystem
										+ " but RS is configured as "
										+ remoteSystem.host
										+ " these names must be equal");
					}
				} else {
					throw new Exception(
							"Server is configured to run with remote system "
									+ server.remoteSystem
									+ " but no RS is configured");
				}
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

	private static void checkFileExists(String path)
			throws FileNotFoundException {
		if (!new File(path).exists() || !new File(path).isFile()) {
			throw new FileNotFoundException("File " + path
					+ " not exist or is not file");
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

	public JBPMBean getJBPM() {
		return jbpm;
	}

	public DBBean getDB() {
		return db;
	}

	public RemoteSystemBean getRemoteSystem() {
		return remoteSystem;
	}

	public SecureStorage getSecureStorage() {
		return secureStorage;
	}
}
