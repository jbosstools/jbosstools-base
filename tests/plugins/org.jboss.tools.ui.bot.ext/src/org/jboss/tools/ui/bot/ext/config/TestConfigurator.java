package org.jboss.tools.ui.bot.ext.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.config.Annotations.ESB;
import org.jboss.tools.ui.bot.ext.config.Annotations.SWTBotTestRequires;
import org.jboss.tools.ui.bot.ext.config.Annotations.Seam;
import org.jboss.tools.ui.bot.ext.config.Annotations.Server;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerState;
import org.jboss.tools.ui.bot.ext.config.Annotations.ServerType;
import org.jboss.tools.ui.bot.ext.config.requirement.RequirementBase;

public class TestConfigurator {
	private static final Logger log = Logger.getLogger(TestConfigurator.class);

	public class Keys {
		public static final String SERVER = "SERVER";
		public static final String SEAM = "SEAM";
		public static final String JAVA = "JAVA";
		public static final String ESB = "ESB";
	}

	public class Values {
		public static final String SERVER_TYPE_EPP = "EPP";
		public static final String SERVER_TYPE_EAP = "EAP";
		public static final String SERVER_TYPE_SOA = "SOA";
		public static final String SERVER_TYPE_JBOSSAS = "JBOSS_AS";
		public static final String SERVER_WITH_DEFAULT_JAVA = "default";
	}

	public static final String SWTBOT_TEST_PROPERTIES_FILE = "swtbot.test.properties.file";
	public static final String SWTBOT_TEST_PROPERTIES_MULTI_FILE = "swtbot.test.properties.multi.file";
	public static Properties multiProperties = new Properties();
	public static TestConfiguration currentConfig;
	static {
		try {
			// try to load from file first
			String propFile = System.getProperty(SWTBOT_TEST_PROPERTIES_FILE,
					null);
			String propMultiFile = System.getProperty(SWTBOT_TEST_PROPERTIES_MULTI_FILE,
					null);
			if (propMultiFile!=null) {
				if (new File(propMultiFile).exists()) {
					log
					.info("Loading exeternaly provided multi-configuration file '"
							+ propMultiFile + "'");
					multiProperties.load(new FileInputStream(propMultiFile));
				}
				else {
					throw new IOException(SWTBOT_TEST_PROPERTIES_MULTI_FILE + " "
							+ propMultiFile + " does not exist!");
				}
			} 
			else if (propFile!=null) {
				if (new File(propFile).exists()) {
					log
					.info("Loading exeternaly configuration file '"
							+ propFile + "'");
					multiProperties.put("Default", propFile);
				}
				else {
					throw new IOException(SWTBOT_TEST_PROPERTIES_FILE + " "
							+ propFile + " does not exist!");
				}
			}
			else {
				log.info("No configuration property passed, using default");
				multiProperties.put(SWTBOT_TEST_PROPERTIES_FILE, "");

			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}
		// load default config by default
		try {
			log.info(" * Loading default configuration first");
			currentConfig = new TestConfiguration("default", "");
			
		} catch (Exception e) {
			// log only message, nothing 
			log.error(e.getMessage());
		}
		finally {
			log.info(" * Defaults loaded");
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
		if (ServerState.Disabled.equals(s.state()) && currentConfig.getServer() == null) {
			return RequirementBase.createRemoveServer();
		}
		if (!s.required() || currentConfig.getServer() == null) {
			return null;
		}
		if (!s.type().equals(ServerType.ALL)) {
			if (s.type().equals(ServerType.EAP)
					&& !currentConfig.getServer().type.equals(Values.SERVER_TYPE_EAP)) {
				return null;
			}
			if (s.type().equals(ServerType.JbossAS)
					&& !currentConfig.getServer().type.equals(Values.SERVER_TYPE_JBOSSAS)) {
				return null;
			}
			if (s.type().equals(ServerType.EPP)
					&& !currentConfig.getServer().type.equals(Values.SERVER_TYPE_EPP)) {
				return null;
			}
			if (s.type().equals(ServerType.SOA)
					&& !currentConfig.getServer().type.equals(Values.SERVER_TYPE_SOA)) {
				return null;
			}
		}
		if (!matches(currentConfig.getServer().version, s.operator(), s.version())) {
			return null;
		}
		if (ServerState.Disabled.equals(s.state())) {
			RequirementBase removeServer = RequirementBase.createRemoveServer();
			removeServer.getDependsOn().add(RequirementBase.createStopServer());
			return removeServer;
		}
		else if (ServerState.NotRunning.equals(s.state())) {
			RequirementBase stopServer = RequirementBase.createStopServer();
			stopServer.getDependsOn().add(RequirementBase.createAddServer());
			return stopServer;
		}
		else if (ServerState.Present.equals(s.state())) {
			return RequirementBase.createAddServer();
		}
		return RequirementBase.createStartServer();
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
			return null;
		}
		if (!matches(currentConfig.getSeam().version, s.operator(), s.version())) {
			return null;
		}
		return RequirementBase.createAddSeam();
	}

	private static RequirementBase getESBRequirement(ESB e) {
		if (!e.required() || currentConfig.getEsb() == null) {
			return null;
		}
		if (!matches(currentConfig.getEsb().version, e.operator(), e.version())) {
			return null;
		}
		return RequirementBase.createAddESB();
	}

	/**
	 * returns list of requirements if given class (Test) can run, all this is
	 * done by exploring class'es annotations (see {@link SWTBotTestRequires} if
	 * class cannot run returns null
	 */
	public static List<RequirementBase> getClassRequirements(Class<?> klass) {

		SWTBotTestRequires requies = klass
				.getAnnotation(SWTBotTestRequires.class);
		// internal list
		List<RequirementBase> reqs = new ArrayList<RequirementBase>();
		reqs.add(RequirementBase.createPrepareViews());
		// all not annotated classes can run
		if (requies == null) {
			return reqs;
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
		int addZeros = 4-version.length();
		if (addZeros>0) {
			while (addZeros>0) {
				version+="0";
				addZeros--;
			}
		}
		return Integer.parseInt(version);
	}

	public static String getProperty(String key) {
		return currentConfig.getProperty(key);
		// return SWTTestExt.util.getValue(swtTestProperties, key);
	}
}
