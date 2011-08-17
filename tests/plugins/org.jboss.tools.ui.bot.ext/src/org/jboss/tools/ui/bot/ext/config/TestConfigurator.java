package org.jboss.tools.ui.bot.ext.config;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.config.Annotations.DB;
import org.jboss.tools.ui.bot.ext.config.Annotations.ESB;
import org.jboss.tools.ui.bot.ext.config.Annotations.JBPM;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Seam;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerLocation;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerState;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerType;
import org.jboss.tools.ui.bot.ext.config.requirement.RequirementBase;

/**
 * This class is main one for holding all possible configurations to run tests on. For every test class
 * it's annotations are read and it is decided whether class is planned or not for given {@link TestConfiguration}
 * 
 * @author lzoubek@redhat.com
 */
public class TestConfigurator {
	private static final Logger log = Logger.getLogger(TestConfigurator.class);
	public static final String RUNTIME_URL_SUFFIX="_URL";
	/**
	 * property keys which are handled in configuration files
	 * 
	 * @author lzoubek
	 *
	 */
	public class Keys {
		public static final String SERVER = "SERVER";
		public static final String SEAM = "SEAM";
		public static final String JAVA = "JAVA";
		public static final String ESB = "ESB";
		public static final String JBPM = "JBPM";
		public static final String DB = "DB";
		public static final String RS = "RS";
		public static final String SS = "SS";
	}
	/**
	 * constant property values which are handled in configuration files
	 * @author lzoubek
	 *
	 */
	public class Values {
		public static final String SERVER_TYPE_EPP = "EPP";
		public static final String SERVER_TYPE_EAP = "EAP";
		public static final String SERVER_TYPE_SOA = "SOA";
		public static final String SERVER_TYPE_JBOSSAS = "JBOSS_AS";
		public static final String SERVER_WITH_DEFAULT_JAVA = "default";
	}
	/**
	 * directory which may contain .properties files to load configurations from
	 */
	public static final String CONFIGURATIONS_DIR="test.configurations.dir";
	/**
	 * regular expression for ignoring property files found in {@link TestConfigurator#CONFIGURATIONS_DIR}
	 * filename matching this will be ignored
	 */
	public static final String CONFIGURATIONS_IGNORE="test.configurations.ignore";
	/**
	 * path to property file which contains either 1 configuration or properties [config name]=[abs path to config property file]
	 */
	public static final String SWTBOT_TEST_PROPERTIES_FILE = "swtbot.test.properties.file";
	public static Properties multiProperties = new Properties();
	public static TestConfiguration currentConfig;
	static {
		boolean loadDefault = true;

		try {
			Pattern configMatch = Pattern.compile("");
			try {
			configMatch = Pattern.compile(System.getProperty(CONFIGURATIONS_IGNORE, ""));
			}
			catch (Exception ex) {
				log.error("Error parsing regex property "+CONFIGURATIONS_IGNORE,ex);
			}
			// try to load from file first
			String propFile = System.getProperty(SWTBOT_TEST_PROPERTIES_FILE,
					null);
			String configsDir=System.getProperty(CONFIGURATIONS_DIR,null);
			if (configsDir != null) {
				File configsDirFile=new File(configsDir);
				if (!configsDirFile.exists()) {
					throw new IOException(CONFIGURATIONS_DIR
							+ " " + configsDir + " does not exist!");
				}
				if (!configsDirFile.isDirectory()) {
					throw new IOException(CONFIGURATIONS_DIR
							+ " " + configsDir + " must be a directory");
				}
				else {
					log.info("Loading property config-files  from '"
							+ configsDir + "'");
					final Pattern fMatch = Pattern.compile(configMatch.toString());
					File[] propFiles = configsDirFile.listFiles(new FileFilter(){

						@Override
						public boolean accept(File  name) {
							return name.getName().endsWith(".properties") && !fMatch.matcher(name.getName()).matches();
						}});
					for (File file : propFiles) 
					{
						log.info("Adding configuration file "+file.getName());
						multiProperties.put(file.getName().replace(".properties", ""), file.getAbsolutePath());						
					}
					loadDefault = false;
				} 
			} else if (propFile != null) {
				if (new File(propFile).exists()) {
					log.info("Loading exeternaly configuration file '"
							+ propFile + "'");
					if (isMultiPropertiesFile(propFile)) {
						log.info(" * loading multi-configuration");
						multiProperties.load(new FileInputStream(propFile));
					}
					else {
						log.info(" * loading single-configuration");
						multiProperties.put("Default", propFile);
					}
					
					loadDefault = false;
				} else {
					throw new IOException(SWTBOT_TEST_PROPERTIES_FILE + " "
							+ propFile + " does not exist!");
				}
			} else {
				log.info("No configuration property passed, using default");
				multiProperties.put(SWTBOT_TEST_PROPERTIES_FILE, "");
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}

		if (loadDefault) {
			// load default config by default
			try {
				log.info(" * Loading default configuration first");
				currentConfig = new TestConfiguration("default", "");

			} catch (Exception e) {
				// log only message, nothing
				log.error(e.getMessage());
			} finally {
				log.info(" * Defaults loaded");
			}
		}

	}
	/**
	 * checks whether given property-file (its conntent) contains multiple configurations to load
	 * or it is just that configuration file
	 * @param propFile
	 * @return true if given property-file (its conntent) contains multiple configurations
	 */
	public static boolean isMultiPropertiesFile(String propFile) {
		Properties props = new Properties();
		try {
			props.load(new FileInputStream(propFile));
			for (Entry<Object,Object> e : props.entrySet()) {
				if (e.getValue()!=null && e.getValue().toString().endsWith(".properties")) {
					return true;
				}
			}
			return false;
		} catch (Exception e) {
			return false;
		} 
	}
	/**
	 * returns null when given Server annotation does not match global test
	 * configuration (e.g. Test wants Server type EAP but we are running on
	 * JbossAS)
	 * 
	 * @param s
	 *            Server annotation
	 * @return StartServer requirement otherwise
	 */
	private static RequirementBase getServerRequirement(Server s) {
		// tests omitting server must run even when server not configured
		if (ServerState.Disabled.equals(s.state())
				&& currentConfig.getServer() == null) {
			return RequirementBase.createRemoveServer();
		}
		if (!s.required() || currentConfig.getServer() == null) {
			ReasonLogger.notConfigured("Server");
			return null;
		}
		if (!s.type().equals(ServerType.ALL)) {
			if (s.type().equals(ServerType.EAP)
					&& !currentConfig.getServer().type
							.equals(Values.SERVER_TYPE_EAP)) {
				ReasonLogger.serverTypeMatch(s.type().toString(), currentConfig.getServer().type);
				return null;
			}
			if (s.type().equals(ServerType.JbossAS)
					&& !currentConfig.getServer().type
							.equals(Values.SERVER_TYPE_JBOSSAS)) {
				ReasonLogger.serverTypeMatch(s.type().toString(), currentConfig.getServer().type);
				return null;
			}
			if (s.type().equals(ServerType.EPP)
					&& !currentConfig.getServer().type
							.equals(Values.SERVER_TYPE_EPP)) {
				ReasonLogger.serverTypeMatch(s.type().toString(), currentConfig.getServer().type);
				return null;
			}
			if (s.type().equals(ServerType.SOA)
					&& !currentConfig.getServer().type
							.equals(Values.SERVER_TYPE_SOA)) {
				ReasonLogger.serverTypeMatch(s.type().toString(), currentConfig.getServer().type);
				return null;
			}
		}
		if (s.location().equals(ServerLocation.Local) && currentConfig.getServer().remoteSystem!=null) {
			ReasonLogger.serverLocation("local", "remote");
			return null;
		}
		if (s.location().equals(ServerLocation.Remote) && currentConfig.getServer().remoteSystem==null) {
			ReasonLogger.serverLocation("remote", "local");
			return null;
		}
		if (!matches(currentConfig.getServer().version, s.operator(),
				s.version())) {
			ReasonLogger.versionMatch("Server", s.operator(), s.version(), currentConfig.getServer().version);
			return null;
		}
		RequirementBase serverReq = null;
		switch (s.state()) {
			case Disabled: {
				serverReq = RequirementBase.createRemoveServer();
				serverReq.getDependsOn().add(RequirementBase.createStopServer());
				break;
			}
			case NotRunning: {
				serverReq = RequirementBase.createStopServer();
				serverReq.getDependsOn().add(RequirementBase.createAddServer());
				break;
			}
			case Present:{
				serverReq = RequirementBase.createAddServer();
				break;
			}
			default:
				serverReq = RequirementBase.createStartServer();
				break;
			}
		return serverReq;
	}
	
	/**
	 * returns null when given Seam annotation does not match global test
	 * configuration (e.g. Test wants Seam version 2.2 but we are running on
	 * 1.2)
	 * 
	 * @param s
	 * @return AddSeam requirement otherwise
	 */
	private static RequirementBase getSeamRequirement(Seam s) {
		if (!s.required() || currentConfig.getSeam() == null) {
			ReasonLogger.notConfigured("Seam");
			return null;
		}
		if (!matches(currentConfig.getSeam().version, s.operator(), s.version())) {
			ReasonLogger.versionMatch("Seam", s.operator(), s.version(), currentConfig.getSeam().version);
			return null;
		}
		return RequirementBase.createAddSeam();
	}

	private static RequirementBase getESBRequirement(ESB e) {
		if (!e.required() || currentConfig.getEsb() == null) {
			ReasonLogger.notConfigured("ESB");
			return null;
		}
		if (!matches(currentConfig.getEsb().version, e.operator(), e.version())) {
			ReasonLogger.versionMatch("ESB", e.operator(), e.version(), currentConfig.getEsb().version);
			return null;
		}
		return RequirementBase.createAddESB();
	}

	private static RequirementBase getJBPMRequirement(JBPM j) {
		if (!j.required() || currentConfig.getJBPM() == null) {
			ReasonLogger.notConfigured("jBPM");
			return null;
		}
		if (!matches(currentConfig.getJBPM().version, j.operator(), j.version())) {
			ReasonLogger.versionMatch("jBPM", j.operator(), j.version(), currentConfig.getJBPM().version);
			return null;
		}
		return RequirementBase.createAddJBPM();
	}

	private static RequirementBase getDBRequirement(DB d) {
		if (!d.required() || currentConfig.getDB() == null) {
			ReasonLogger.notConfigured("DB");
			return null;
		}
		if (!matches(currentConfig.getDB().version, d.operator(), d.version())) {
			ReasonLogger.versionMatch("DB", d.operator(), d.version(), currentConfig.getDB().version);
			return null;
		}
		return RequirementBase.prepareDB();
	}
	/**
	 * returns true if given class requires any of all possible runtimes
	 * @param klass
	 * @return
	 */
	public static boolean isRequiresAnyRuntime(Class<?> klass) {
		SWTBotTestRequires an = getAnnotation(klass);
		if (an==null) {
			return false;
		}
		return an.db().required() || an.esb().required()
		|| an.jbpm().required() || an.seam().required()
		|| an.server().required();
	}
	/**
	 * returns true if given class has {@link SWTBotTestRequires#runOnce()} annotation set to true
	 * @param klass
	 * @return
	 */
	public static boolean isRequiresRunOnce(Class<?> klass) {
		SWTBotTestRequires an = getAnnotation(klass);
		if (an==null) {
			return false;
		}
		return an.runOnce();
	}
	/**
	 * finds {@link SWTBotTestRequires} annotation in given class or recursive in super classes 
	 * @param klass
	 * @return
	 */
	private static SWTBotTestRequires getAnnotation(Class<?> klass) {
		if (klass==null || Object.class.equals(klass)) {
			return null;
		}
		SWTBotTestRequires requies = klass
		.getAnnotation(SWTBotTestRequires.class);
		if (requies != null) {
			return requies;			
		}
		return getAnnotation(klass.getSuperclass());
	}
	/**
	 * returns list of requirements if given class (Test) can run, all this is
	 * done by exploring class'es annotations (see {@link SWTBotTestRequires}) and check against
	 * current configuration   
	 * if given class does not meet {@link TestConfigurator#currentConfig} method returns null
	 */
	public static List<RequirementBase> getClassRequirements(Class<?> klass) {

		SWTBotTestRequires requies = getAnnotation(klass);
		// internal list
		List<RequirementBase> reqs = new ArrayList<RequirementBase>();
		reqs.add(RequirementBase.createPrepareViews());
		reqs.add(RequirementBase.createSetProperties());
		// all not annotated classes can run
		if (requies == null) {
			return reqs;
		}
		if (requies.secureStorage() && currentConfig.getSecureStorage()==null) {
			ReasonLogger.notConfigured("Secure Storage");
			return null;
		}
		if (requies.server().required()) {
			RequirementBase req = getServerRequirement(requies.server());
			if (req == null) {
				return null;
			}
			reqs.add(req);
		}
		if (requies.seam().required()) {
			RequirementBase req = getSeamRequirement(requies.seam());
			if (req == null) {
				return null;
			}
			reqs.add(req);
		}
		if (requies.esb().required()) {
			RequirementBase req = getESBRequirement(requies.esb());
			if (req == null) {
				return null;
			}
			reqs.add(req);
		}
		if (requies.jbpm().required()) {
			RequirementBase req = getJBPMRequirement(requies.jbpm());
			if (req == null) {
				return null;
			}
			reqs.add(req);
		}
		if (requies.db().required()) {
			RequirementBase req = getDBRequirement(requies.db());
			if (req == null) {
				return null;
			}
			reqs.add(req);
		}

		if (!"".equals(requies.perspective())) {
			reqs.add(RequirementBase.createSwitchPerspective(requies
					.perspective()));
		}
		if (requies.clearWorkspace()) {
			reqs.add(RequirementBase.createClearWorkspace());
		}
		if (requies.clearProjects()) {
			reqs.add(RequirementBase.createClearProjects());
		}
		// sort requirements by priority
		Collections.sort(reqs, new Comparator<RequirementBase>() {
			public int compare(RequirementBase o1, RequirementBase o2) {
				return o1.getPriority() - o2.getPriority();
			}
		});

		return reqs;
	}

	/**
	 * implements comparison of 2 params by given operator (in this order)
	 * params are expected version strings (in form X.X or XX) if param1 or
	 * param2 is '*' true is returned
	 * 
	 * @param param1
	 * @param operator
	 *            (=,<,>=<=,>=,!=)
	 * @param param2
	 * 
	 * @return
	 */
	public static boolean matches(String param1, String operator, String param2) {
		if ("*".equals(param1) || "*".equals(param2)) {
			return true;
		}
		if ("=".equals(operator)) {
			return param1.equals(param2);
		}
		if ("!=".equals(operator)) {
			return !param1.equals(param2);
		}
		int ver1 = versionToNumber(param1);
		int ver2 = versionToNumber(param2);
		if (">".equals(operator)) {
			return ver1 > ver2;
		}
		if (">=".equals(operator)) {
			return ver1 >= ver2;
		}
		if ("<".equals(operator)) {
			return ver1 < ver2;
		}
		if ("<=".equals(operator)) {
			return ver1 <= ver2;
		}
		return false;
	}

	private static int versionToNumber(String version) {
		version = version.replaceAll("\\.", "");
		int addZeros = 4 - version.length();
		if (addZeros > 0) {
			while (addZeros > 0) {
				version += "0";
				addZeros--;
			}
		}
		return Integer.parseInt(version);
	}

	public static String getProperty(String key) {
		return currentConfig.getProperty(key);
		// return SWTTestExt.util.getValue(swtTestProperties, key);
	}
	/**
	 * internal class providing methods for logging reasons explaining which annotation
	 * requirements have not been met and why a testclass was not executed
	 * @author lzoubek
	 *
	 */
	private static class ReasonLogger {

		public static void notConfigured(String what) {
			log.info(String.format("Requires %s, but it is not configured", what));
		}
		public static void serverTypeMatch(String required, String configured) {
			log.info(String.format("Requires %s server type, but configured is %s",required,configured));
		}
		public static void serverLocation(String required, String configured) {
			log.info(String.format("Requires %s server location, but is configured as %s",required,configured));
		}
		public static void versionMatch(String what, String operator, String required, String configured) {
			log.info(String.format("Required %s %s %s version  does not match, configured is %s",operator,required,what,configured));
		}
	}
}
