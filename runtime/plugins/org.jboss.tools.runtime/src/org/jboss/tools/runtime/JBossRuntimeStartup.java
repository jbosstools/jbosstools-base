package org.jboss.tools.runtime;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

import org.drools.eclipse.util.DroolsRuntime;
import org.drools.eclipse.util.DroolsRuntimeManager;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
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
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeType;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerType;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerCore;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.internal.RuntimeWorkingCopy;
import org.eclipse.wst.server.core.internal.ServerWorkingCopy;
import org.jboss.ide.eclipse.as.core.util.JBossServerType;
import org.jboss.ide.eclipse.as.core.util.ServerBeanLoader;
import org.jboss.tools.jbpm.preferences.JbpmInstallation;
import org.jboss.tools.jbpm.preferences.PreferencesManager;
import org.jboss.tools.seam.core.SeamCorePlugin;
import org.jboss.tools.seam.core.SeamUtil;
import org.jboss.tools.seam.core.project.facet.SeamRuntime;
import org.jboss.tools.seam.core.project.facet.SeamRuntimeManager;
import org.jboss.tools.seam.core.project.facet.SeamVersion;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;

public class JBossRuntimeStartup implements IStartup {

	/**
	 * 
	 */
	private static final String DEFAULT_DS = "DefaultDS";

	private static final String RUNTIME = Messages.JBossRuntimeStartup_Runtime;

	private static final String EAP = "EAP"; //$NON-NLS-1$

	private static final String SOA_P = "SOA-P"; //$NON-NLS-1$
	
	private static final String SOA_P_STD = "SOA-P-STD"; //$NON-NLS-1$
	
	private static final String EPP = "EPP"; //$NON-NLS-1$
	
	private static final String EWP = "EWP"; //$NON-NLS-1$
	
	public static final String SEAM = "SEAM"; // NON-NLS-1$
	
	public static final String DROOLS = "DROOLS"; // NON-NLS-1$
	
	private static final String AS = "AS"; //$NON-NLS-1$

	public static final String JBOSS_EAP_HOME = "../../../../jboss-eap/jboss-as"; 	// JBoss AS home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	
	public static final String JBOSS_EAP_HOME_CONFIGURATION = "../../jboss-eap/jboss-as"; 	// JBoss AS home directory (relative to plugin)- <RHDS_HOME>/jbossas. //$NON-NLS-1$
	
	public static final String SERVERS_FILE_NAME = "application_platforms.properties"; //$NON-NLS-1$
	
	public static final String SERVERS_FILE = "../../../../studio/" + SERVERS_FILE_NAME; //$NON-NLS-1$
	
	public static final String SERVERS_FILE_CONFIGURATION = "../../studio/" + SERVERS_FILE_NAME; //$NON-NLS-1$
	
	public static String RUNTIME_CONFIG_FORMAT_VERSION = "1.0"; //$NON-NLS-1$
	
	public static final String SEAM_1_2_HOME = "../../../../jboss-eap/seam";  //$NON-NLS-1$
	public static final String SEAM_1_2_HOME_CONFIGURATION = "../../jboss-eap/seam";  //$NON-NLS-1$
	public static final String SEAM_1_2_HOME_CP = "../../../../jboss-eap/seam1";  //$NON-NLS-1$
	public static final String SEAM_1_2_HOME_CONFIGURATION_CP = "../../jboss-eap/seam1";  //$NON-NLS-1$

	public static final String SEAM_2_0_HOME = "../../../../jboss-eap/seamfp";  //$NON-NLS-1$
	public static final String SEAM_2_0_HOME_CONFIGURATION = "../../jboss-eap/seamfp";  //$NON-NLS-1$
	public static final String SEAM_2_0_HOME_CP = "../../../../jboss-eap/seam2";  //$NON-NLS-1$
	public static final String SEAM_2_0_HOME_CONFIGURATION_CP = "../../jboss-eap/seam2";  //$NON-NLS-1$
	
	public static final String[] SEAM_HOME_FOLDER_OPTIONS = {"seam","seam1","seam2","seamfp"};
	
	// This constants are made to avoid dependency with org.jboss.ide.eclipse.as.core plugin
	public static final String JBOSS_AS_RUNTIME_TYPE_ID[] = {
		"org.jboss.ide.eclipse.as.runtime.32", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.40", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.42", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.51", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.60", //$NON-NLS-1$		
		"org.jboss.ide.eclipse.as.runtime.eap.43", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.runtime.eap.50" //$NON-NLS-1$
		};
	
	public static final String HSQLDB_DRIVER_JAR_NAME = "hsqldb.jar"; //$NON-NLS-1$
	
	public static final String HSQLDB_DRIVER_3X_4X_LOCATION = "/server/default/lib/" + HSQLDB_DRIVER_JAR_NAME; //$NON-NLS-1$
	
	public static final String HSQLDB_DRIVER_5X_LOCATION = "/common/lib/" + HSQLDB_DRIVER_JAR_NAME; //$NON-NLS-1$
	
	// This constants are made to avoid dependency with org.jboss.ide.eclipse.as.core plugin
	public static final String JBOSS_AS_HSQL_DRIVER_LOCATION[] = {
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION,		
		HSQLDB_DRIVER_3X_4X_LOCATION,
		HSQLDB_DRIVER_5X_LOCATION
	};

	public static final String JBOSS_AS_TYPE_ID[] = {
		"org.jboss.ide.eclipse.as.32", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.40", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.42", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.50", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.51", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.60", //$NON-NLS-1$		
		"org.jboss.ide.eclipse.as.eap.43", //$NON-NLS-1$
		"org.jboss.ide.eclipse.as.eap.50", //$NON-NLS-1$
		};
	
	public static final String JBOSS_AS_NAME[] = {
		Messages.JBossRuntimeStartup_JBoss_Application_Server_3_2,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_4_0,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_4_2,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_5_0,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_5_1,
		Messages.JBossRuntimeStartup_JBoss_Application_Server_6_0,
		Messages.JBossRuntimeStartup_JBoss_EAP_Server_4_3,
		Messages.JBossRuntimeStartup_JBoss_EAP_Server_5_0
		};
	
	public static final String JBOSS_AS_HOST = "localhost"; //$NON-NLS-1$

	public static final String JBOSS_AS_DEFAULT_CONFIGURATION_NAME = "default"; //$NON-NLS-1$

	//public static final String FIRST_START_PREFERENCE_NAME = "FIRST_START";

	public static final String HSQL_DRIVER_DEFINITION_ID 
												= "DriverDefn.Hypersonic DB"; //$NON-NLS-1$

	public static final String HSQL_DRIVER_NAME = "Hypersonic DB"; //$NON-NLS-1$

	public static final String HSQL_DRIVER_TEMPLATE_ID 
						= "org.eclipse.datatools.enablement.hsqldb.1_8.driver"; //$NON-NLS-1$

	public static final String DTP_DB_URL_PROPERTY_ID 
								= "org.eclipse.datatools.connectivity.db.URL"; //$NON-NLS-1$

	private static final String HSQL_PROFILE_ID = "org.eclipse.datatools.enablement.hsqldb.connectionProfile";

	public static final String JBPM = "JBPM";
	
	private List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();

	private IEclipsePreferences preferences;
	
	public void earlyStartup() {
		if (!isJBDS()) {
			return;
		}
		if (!willBeInitialized()) {
			return;
		}
		parseServerFile();
		initializeRuntimes(serverDefinitions);
		saveWorkspacePreferences();
	}

	private void saveWorkspacePreferences() {
		Activator.getDefault().getPreferenceStore().setValue(Activator.FIRST_START, false);
		String workspaces = getWorkspaces();
		String newWorkspaces = "";
		boolean addWorkspace = true;
		if (workspaces == null || workspaces.trim().length() == 0) {
			newWorkspaces = getWorkspace();
		} else {
			StringTokenizer tokenizer = new StringTokenizer(workspaces, ",");
			while (tokenizer.hasMoreTokens()) {
				String workspace = tokenizer.nextToken();
				if (workspace.equals(getWorkspace())) {
					addWorkspace = false;
				}
			}
			if (addWorkspace) {
				newWorkspaces = workspaces + "," + getWorkspace();
			}
		}
		if (addWorkspace) {
			IEclipsePreferences prefs = getPreferences();
			prefs.put(Activator.WORKSPACES, newWorkspaces);
			try {
				prefs.flush();
			} catch (BackingStoreException e) {
				Activator.log(e);
			}
		}
	}

	/**
	 * @return
	 */
	private boolean willBeInitialized() {
		boolean firstStart = Activator.getDefault().getPreferenceStore().getBoolean(Activator.FIRST_START);
		if (firstStart) {
			return true;
		}
	
		String workspaces = getWorkspaces();
		if (workspaces == null || workspaces.trim().length() == 0) {
			return true;
		}
		StringTokenizer tokenizer = new StringTokenizer(workspaces, ",");
		while (tokenizer.hasMoreTokens()) {
			String workspace = tokenizer.nextToken();
			if (workspace.equals(getWorkspace())) {
				return false;
			}
		}
		return true;
	}

	private String getWorkspaces() {
		IEclipsePreferences prefs = getPreferences();
		String workspaces = prefs.get(Activator.WORKSPACES, "");
		return workspaces;
	}

	private String getWorkspace() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath workspacePath = root.getLocation();
		return workspacePath.toOSString();
	}

	private IEclipsePreferences getPreferences() {
		if (preferences == null) {
			preferences = new ConfigurationScope().getNode(Activator.PLUGIN_ID);
		}
		return preferences;
	}

	private boolean isJBDS() {
		Bundle jbdsProduct = Platform.getBundle("com.jboss.jbds.product");
		return jbdsProduct != null;
	}

	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		initializeJbpmRuntime(serverDefinitions);
		initializeDroolsRuntime(serverDefinitions);
		try {
			String pluginLocation = FileLocator.resolve(Activator.getDefault().getBundle().getEntry("/")).getPath(); //$NON-NLS-1$
			File jbossASDir = new File(pluginLocation, JBOSS_EAP_HOME);
			if (!jbossASDir.isDirectory()) {
				Location configLocation = Platform.getConfigurationLocation();
				URL configURL = configLocation.getURL();
				String configuration = FileLocator.resolve(configURL).getPath();
				jbossASDir = new File(configuration, JBOSS_EAP_HOME_CONFIGURATION).getCanonicalFile();
			} else {
				jbossASDir = jbossASDir.getCanonicalFile();
			}
			if (jbossASDir.isDirectory()) {
				int index = getJBossASVersion(jbossASDir);
				createJBossServer(jbossASDir,index, "jboss-eap", "jboss-eap " + RUNTIME); //$NON-NLS-1$ //$NON-NLS-2$
			}
		} catch (IOException e) {
			log(e,Messages.JBossRuntimeStartup_Cannot_create_new_JBoss_Server);
		}
		createJBossServer(serverDefinitions);
		
		initializeSeam(serverDefinitions);
		// https://jira.jboss.org/jira/browse/JBDS-1091
		Display.getDefault().asyncExec(new Runnable() {
			
			public void run() {
				IViewPart view = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.wst.server.ui.ServersView");
				if (view instanceof CommonNavigator) {
					CommonNavigator navigator = (CommonNavigator) view;
					navigator.getCommonViewer().refresh();
				}
			}
		});
	}

	public void initializeSeam(List<ServerDefinition> serverDefinitions) {
		
		Map<String, SeamRuntime> map = new HashMap<String,SeamRuntime>();

		// to fix https://jira.jboss.org/jira/browse/JBDS-682
		// seam runtime initialization goes throug added servers first and 
		// then process seam runtimes from bundled servers
		for(ServerDefinition serverDefinition:serverDefinitions) {
			String type = serverDefinition.getType();
			if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type) || EWP.equals(type) ) {
				for (String folder : SEAM_HOME_FOLDER_OPTIONS) {
					File seamFile = new File(serverDefinition.getLocation(),folder); //$NON-NLS-1$
					addSeam(map, serverDefinition, seamFile);
				} 
			}
			if (SEAM.equals(type)) {
				addSeam(map, serverDefinition, serverDefinition.getLocation());
			}
		}

		// Initialize Seam Runtime from JBoss EAP
		String seamGenBuildPath = getSeamGenBuildPath(SEAM_1_2_HOME, SEAM_1_2_HOME_CONFIGURATION);
		SeamVersion seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam1(map, seamGenBuildPath, seamVersion);
		
		// fix for https://jira.jboss.org/jira/browse/JBDS-1215 The installer could not find EAP 4.3 Seam runtimes in recent CP
		seamGenBuildPath = getSeamGenBuildPath(SEAM_1_2_HOME_CP, SEAM_1_2_HOME_CONFIGURATION_CP);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam1(map, seamGenBuildPath, seamVersion);

		// Initialize Seam 2.0 Runtime from JBoss EAP
		seamGenBuildPath = getSeamGenBuildPath(SEAM_2_0_HOME, SEAM_2_0_HOME_CONFIGURATION);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam2(map, seamGenBuildPath, seamVersion);
		
		// fix for https://jira.jboss.org/jira/browse/JBDS-1215 The installer could not find EAP 4.3 Seam runtimes in recent CPs
		seamGenBuildPath = getSeamGenBuildPath(SEAM_2_0_HOME_CP, SEAM_2_0_HOME_CONFIGURATION_CP);
		seamVersion = getSeamVersion(seamGenBuildPath);
		addSeam2(map, seamGenBuildPath, seamVersion);
	}

	private void addSeam1(Map<String, SeamRuntime> map,
			String seamGenBuildPath, SeamVersion seamVersion) {
		if (seamVersion != null) {
			StringBuffer name = new StringBuffer("Seam ").append(seamVersion); //$NON-NLS-1$
			if(seamVersion.compareTo(SeamVersion.SEAM_1_2)==0) {
				name.append(".EAP_4.3"); //$NON-NLS-1$
			} else {
				name.append(".EAP5"); //$NON-NLS-1$
			}
			addSeam(map, seamGenBuildPath,seamVersion,name.toString());
		}
	}

	private void addSeam2(Map<String, SeamRuntime> map,
			String seamGenBuildPath, SeamVersion seamVersion) {
		if (seamVersion != null) {
			String name = "Seam " + seamVersion + ".FP"; //$NON-NLS-1$ //$NON-NLS-2$
			addSeam(map, seamGenBuildPath, seamVersion,name);
		}
	}

	private void addSeam(Map<String, SeamRuntime> map,
			ServerDefinition serverDefinition, File seamFile) {
		if (seamFile.exists() && seamFile.canRead() && seamFile.isDirectory()) {
			SeamVersion seamVersion = getSeamVersion(seamFile.getAbsolutePath());
			if (seamVersion != null) {
				String name = "Seam " + serverDefinition.getName() + " " + seamVersion; //$NON-NLS-1$ //$NON-NLS-2$
				addSeam(map, seamFile.getAbsolutePath(), seamVersion, name);
			}
		}
	}

	private SeamVersion getSeamVersion(String seamGenBuildPath) {
		if (seamGenBuildPath == null || seamGenBuildPath.trim().length() <= 0) {
			return null;
		}
		String fullVersion = SeamUtil.getSeamVersionFromManifest(seamGenBuildPath);
		if (fullVersion == null) {
			return null;	
		}
		String version = fullVersion.substring(0,3);
		SeamVersion seamVersion = null;
		if (version != null) {
			seamVersion = SeamVersion.findByString(version);
		}
		return seamVersion;
	}

	private void addSeam(Map<String, SeamRuntime> map, String seamPath,SeamVersion seamVersion, String name) {
		if (!seamExists(seamPath)) {
			File seamFolder = new File(seamPath);
			if(seamFolder.exists() && seamFolder.isDirectory()) {
				SeamRuntime rt = new SeamRuntime();
				rt.setHomeDir(seamPath);
				rt.setName(name);
				rt.setDefault(true);
				rt.setVersion(seamVersion);
				SeamRuntimeManager.getInstance().addRuntime(rt);
			}
		}
	}

	/**
	 * @param seamPath
	 * @return
	 */
	private boolean seamExists(String seamPath) {
		SeamRuntime[] seamRuntimes = SeamRuntimeManager.getInstance().getRuntimes();
		for (SeamRuntime sr:seamRuntimes) {
			if (seamPath != null && seamPath.equals(sr.getHomeDir())) {
				return true;
			}
		}
		return false;
	}

	private String getSeamGenBuildPath(String seamHomePath,
			String seamHomePathConfiguration) {
		try {
			Location configLocation = Platform.getConfigurationLocation();
			URL configURL = configLocation.getURL();
			String configuration = FileLocator.resolve(configURL).getPath();
			File seamGenDir = new File(configuration, seamHomePathConfiguration);
			if (!seamGenDir.isDirectory()) {
				String pluginLocation = null;
				pluginLocation = FileLocator
						.resolve(
								Activator.getDefault().getBundle()
										.getEntry("/")).getFile(); //$NON-NLS-1$
				seamGenDir = new File(pluginLocation, seamHomePath);
			}
			Path p = new Path(seamGenDir.getPath());
			p.makeAbsolute();
			if (p.toFile().exists()) {
				return p.toOSString();
			}
		} catch (IOException e) {
			log(e);
		}
		return ""; //$NON-NLS-1$
	}
	
	public void createJBossServer(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition:serverDefinitions) {
			String type = serverDefinition.getType();
			if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type) || SOA_P_STD.equals(type) || EWP.equals(type)) {
				File asLocation = new File(serverDefinition.getLocation(), "jboss-as");
				if(SOA_P_STD.equals(type)) {
					asLocation = new File(serverDefinition.getLocation(),"jboss-esb"); //$NON-NLS-1$					
				} else if(EWP.equals(type)){
					asLocation = new File(serverDefinition.getLocation(),"jboss-as-web"); //$NON-NLS-1$
				}
				if (asLocation.isDirectory()) {
					String name = serverDefinition.getName();
					String runtimeName = name + " " + RUNTIME; //$NON-NLS-1$
					int index = getJBossASVersion(asLocation);
					createJBossServer(asLocation,index,name, runtimeName);
				}
			} else if (AS.equals(type)){
				String version = serverDefinition.getVersion();
				int index = 2;
				if ("3.2".equals(version)) { //$NON-NLS-1$
					index = 0;
				} else if ("4.0".equals(version)) { //$NON-NLS-1$
					index = 1;
				} else if ("4.2".equals(version)) { //$NON-NLS-1$
					index = 2;
				} else if ("5.0".equals(version)) { //$NON-NLS-1$
					index = 3;
				} else if ("5.1".equals(version)) { //$NON-NLS-1$
					index = 4;
				} else if ("6.0".equals(version)) { //$NON-NLS-1$
					index = 5;
				}
				createJBossServer(serverDefinition.getLocation(),index,serverDefinition.getName(),serverDefinition.getName() + " " + RUNTIME); //$NON-NLS-1$
			}
		}	
	}

	private int getJBossASVersion(File asLocation) {
		int index = -1;
		String fullVersion = new ServerBeanLoader().getFullServerVersion(new File(asLocation, JBossServerType.AS.getSystemJarPath()));
		if(fullVersion != null ) {
			String version = fullVersion.substring(0, 3);
			if ("4.3".equals(version)) { //$NON-NLS-1$
				index = 6;
			} else if ("5.0".equals(version)) { //$NON-NLS-1$
				index = 7;
			} else if ("5.1".equals(version)) { //$NON-NLS-1$
				// FIXME - this needs to be changed when adding a new runtime type for JBoss EAP 5.1
				index = 7;
			} 
		}
		return index;
	}

	private void createJBossServer(File asLocation, int index, String name, String runtimeName) {
		if (!asLocation.isDirectory() || index==-1) {
			return;
		}
		IPath jbossAsLocationPath = new Path(asLocation.getAbsolutePath());

		IServer[] servers = ServerCore.getServers();
		for (int i = 0; i < servers.length; i++) {
			IRuntime runtime = servers[i].getRuntime();
			if(runtime != null && runtime.getLocation().equals(jbossAsLocationPath)) {
				return;
			}
		}

		IRuntime runtime = null;
		IRuntime[] runtimes = ServerCore.getRuntimes();
		for (int i = 0; i < runtimes.length; i++) {
			if (runtimes[0].getLocation().equals(jbossAsLocationPath)) {
				runtime = runtimes[0].createWorkingCopy();
				break;
			}
		}

		IProgressMonitor progressMonitor = new NullProgressMonitor();
		try {
			if (runtime == null) {
				runtime = createRuntime(runtimeName, asLocation.getAbsolutePath(), progressMonitor, index);
			}
			if (runtime != null) {
				createServer(progressMonitor, runtime, index, name);
			}

			createDriver(asLocation.getAbsolutePath(), index);
		} catch (CoreException e) {
			log(e,Messages.JBossRuntimeStartup_Cannot_create_new_JBoss_Server);
		} catch (ConnectionProfileException e) {
			log(e,Messages.JBossRuntimeStartup_Cannott_create_new_DTP_Connection_Profile);
		}
	}

	private void parseServerFile() {
		
		try {
			String pluginLocation = FileLocator.resolve(Activator.getDefault().getBundle().getEntry("/")).getPath(); //$NON-NLS-1$
			File serversFile = new File(pluginLocation, SERVERS_FILE);

			if (!serversFile.isFile()) {
				Location configLocation = Platform.getConfigurationLocation();
				URL configURL = configLocation.getURL();
				String configuration = FileLocator.resolve(configURL).getPath();
				serversFile = new File(configuration, SERVERS_FILE_CONFIGURATION).getCanonicalFile();
			} else {
				serversFile = serversFile.getCanonicalFile();
			}
			if (!serversFile.isFile()) {
				serversFile = new File(pluginLocation,SERVERS_FILE_NAME);
			}
			if (serversFile.isFile()) {
				//String str = FileUtil.readFile(serversFile);
				Properties servers = new Properties();
				servers.load(new BufferedInputStream(new FileInputStream(serversFile)));
				Enumeration<Object> elements = servers.elements();
				while (elements.hasMoreElements()) {
					String str = (String) elements.nextElement();
					StringTokenizer lineTokenizer = new StringTokenizer(str,
							"\n\r\f"); //$NON-NLS-1$
					while (lineTokenizer.hasMoreTokens()) {
						String lineToken = lineTokenizer.nextToken();
						StringTokenizer tokenizer = new StringTokenizer(
								lineToken, ","); //$NON-NLS-1$
						if (tokenizer.countTokens() == 4) {
							String name = tokenizer.nextToken();
							/*int index = name.indexOf('=');
							if (index < 0) {
								continue;
							}
							name = name.substring(index + 1);*/
							String type = tokenizer.nextToken();
							String version = tokenizer.nextToken();
							String location = tokenizer.nextToken();
							File locationFile = new File(location);
							if (locationFile.isDirectory()) {
								serverDefinitions.add(new ServerDefinition(
										name, version, type, locationFile));
							}
						}
					}
				}
			}
		} catch (FileNotFoundException e) {
			log(e);
		} catch (IOException e) {
			log(e);
		}
	}
	
	private static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
				.getLocalizedMessage(), e);
		Activator.getDefault().getLog().log(status);
	}
	
	private static void log(Throwable e, String message) {
		IStatus status = new Status(IStatus.ERROR, Activator.PLUGIN_ID, message, e);
		Activator.getDefault().getLog().log(status);
	}
	
	/**
	 * Creates new JBoss AS Runtime, Server and hsqldb driver
	 * @param jbossASLocation location of JBoss Server
	 * @param progressMonitor to report progress
	 * @return server working copy
	 * @throws CoreException
	 * @throws ConnectionProfileException
	 */
//	public static IServerWorkingCopy initJBossAS(String jbossASLocation, IProgressMonitor progressMonitor) throws CoreException, ConnectionProfileException {
//		IRuntime runtime = createRuntime(null, jbossASLocation, progressMonitor, 2);
//		IServerWorkingCopy server = null;
//		if (runtime != null) {
//			server = createServer(progressMonitor, runtime, 2, null);
//		}
//		createDriver(jbossASLocation);
//		return server;
//	}

	/**
	 * Creates new JBoss AS Runtime
	 * @param jbossASLocation location of JBoss AS
	 * @param progressMonitor
	 * @return runtime working copy
	 * @throws CoreException
	 */
	private static IRuntime createRuntime(String runtimeName, String jbossASLocation, IProgressMonitor progressMonitor, int index) throws CoreException {
		IRuntimeWorkingCopy runtime = null;
		String type = null;
		String version = null;
		String runtimeId = null;
		IPath jbossAsLocationPath = new Path(jbossASLocation);
		IRuntimeType[] runtimeTypes = ServerUtil.getRuntimeTypes(type, version, JBOSS_AS_RUNTIME_TYPE_ID[index]);
		if (runtimeTypes.length > 0) {
			runtime = runtimeTypes[0].createRuntime(runtimeId, progressMonitor);
			runtime.setLocation(jbossAsLocationPath);
			if(runtimeName!=null) {
				runtime.setName(runtimeName);				
			}
//			to fix https://jira.jboss.org/jira/browse/JBDS-852 VM attributes initialization below was commented
//			IVMInstall defaultVM = JavaRuntime.getDefaultVMInstall();
			// IJBossServerRuntime.PROPERTY_VM_ID
//			((RuntimeWorkingCopy) runtime).setAttribute("PROPERTY_VM_ID", defaultVM.getId()); //$NON-NLS-1$
			// IJBossServerRuntime.PROPERTY_VM_TYPE_ID
//			((RuntimeWorkingCopy) runtime).setAttribute("PROPERTY_VM_TYPE_ID", defaultVM.getVMInstallType().getId()); //$NON-NLS-1$
			// IJBossServerRuntime.PROPERTY_CONFIGURATION_NAME
			((RuntimeWorkingCopy) runtime).setAttribute("org.jboss.ide.eclipse.as.core.runtime.configurationName", JBOSS_AS_DEFAULT_CONFIGURATION_NAME); //$NON-NLS-1$

			return runtime.save(false, progressMonitor);
		}
		return runtime;
	}

	/**
	 * Creates new JBoss Server
	 * @param progressMonitor
	 * @param runtime parent JBoss AS Runtime
	 * @return server working copy
	 * @throws CoreException
	 */
	private static IServerWorkingCopy createServer(IProgressMonitor progressMonitor, IRuntime runtime, int index, String name) throws CoreException {
		if (name == null) {
			name = JBOSS_AS_NAME[index];
		}
		IServer[] servers = ServerCore.getServers();
		for (IServer server:servers) {
			if (name.equals(server.getName()) ) {
				return null;
			}
		}
		IServerType serverType = ServerCore.findServerType(JBOSS_AS_TYPE_ID[index]);
		IServerWorkingCopy server = serverType.createServer(null, null, runtime, progressMonitor);

		server.setHost(JBOSS_AS_HOST);
		server.setName(name);
		
		// JBossServer.DEPLOY_DIRECTORY
		String deployVal = runtime.getLocation().append("server").append(JBOSS_AS_DEFAULT_CONFIGURATION_NAME).append("deploy").toOSString(); //$NON-NLS-1$ //$NON-NLS-2$
		((ServerWorkingCopy) server).setAttribute("org.jboss.ide.eclipse.as.core.server.deployDirectory", deployVal); //$NON-NLS-1$

		// IDeployableServer.TEMP_DEPLOY_DIRECTORY
		String deployTmpFolderVal = runtime.getLocation().append("server").append(JBOSS_AS_DEFAULT_CONFIGURATION_NAME).append("tmp").append("jbosstoolsTemp").toOSString(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		((ServerWorkingCopy) server).setAttribute("org.jboss.ide.eclipse.as.core.server.tempDeployDirectory", deployTmpFolderVal); //$NON-NLS-1$

		// If we'd need to set up a username / pw for JMX, do it here.
//		((ServerWorkingCopy)serverWC).setAttribute(JBossServer.SERVER_USERNAME, authUser);
//		((ServerWorkingCopy)serverWC).setAttribute(JBossServer.SERVER_PASSWORD, authPass);

		server.save(false, progressMonitor);
		return server;
	}

	/**
	 * Creates HSQL DB Driver
	 * @param jbossASLocation location of JBoss AS
	 * @param index 
	 * @throws ConnectionProfileException
	 * @return driver instance
	 */
	private static void createDriver(String jbossASLocation, int index) throws ConnectionProfileException {
		if(ProfileManager.getInstance().getProfileByName(DEFAULT_DS) != null) {
			// Don't create the driver a few times
			return;
		}
		String driverPath;
		try {
			driverPath = new File(jbossASLocation + JBOSS_AS_HSQL_DRIVER_LOCATION[index]).getCanonicalPath(); //$NON-NLS-1$
		} catch (IOException e) {
			Activator.getDefault().getLog().log(new Status(IStatus.ERROR,
					Activator.PLUGIN_ID, Messages.JBossRuntimeStartup_Cannott_create_new_HSQL_DB_Driver, e));
			return;
		}

		DriverInstance driver = DriverManager.getInstance().getDriverInstanceByName(HSQL_DRIVER_NAME);
		if (driver == null) {
			TemplateDescriptor descr = TemplateDescriptor.getDriverTemplateDescriptor(HSQL_DRIVER_TEMPLATE_ID);
			IPropertySet instance = new PropertySetImpl(HSQL_DRIVER_NAME, HSQL_DRIVER_DEFINITION_ID);
			instance.setName(HSQL_DRIVER_NAME);
			instance.setID(HSQL_DRIVER_DEFINITION_ID);
			Properties props = new Properties();

			IConfigurationElement[] template = descr.getProperties();
			for (int i = 0; i < template.length; i++) {
				IConfigurationElement prop = template[i];
				String id = prop.getAttribute("id"); //$NON-NLS-1$

				String value = prop.getAttribute("value"); //$NON-NLS-1$
				props.setProperty(id, value == null ? "" : value); //$NON-NLS-1$
			}
			props.setProperty(DTP_DB_URL_PROPERTY_ID, "jdbc:hsqldb:."); //$NON-NLS-1$
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_TYPE, descr.getId());
			props.setProperty(IDriverMgmtConstants.PROP_DEFN_JARLIST, driverPath);

			instance.setBaseProperties(props);
			DriverManager.getInstance().removeDriverInstance(instance.getID());
			System.gc();
			DriverManager.getInstance().addDriverInstance(instance);
		}

		driver = DriverManager.getInstance().getDriverInstanceByName(HSQL_DRIVER_NAME);
		if (driver != null && ProfileManager.getInstance().getProfileByName(DEFAULT_DS) == null) { //$NON-NLS-1$
			// create profile
			Properties props = new Properties();
			props.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, HSQL_DRIVER_DEFINITION_ID);
			props.setProperty(IDBConnectionProfileConstants.CONNECTION_PROPERTIES_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DRIVER_CLASS_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID,	driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VENDOR_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.DATABASE_VERSION_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.DATABASE_NAME_PROP_ID, "Default"); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.PASSWORD_PROP_ID, ""); //$NON-NLS-1$
			props.setProperty(IDBConnectionProfileConstants.SAVE_PASSWORD_PROP_ID, "false"); //$NON-NLS-1$
			props.setProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.USERNAME_PROP_ID));
			props.setProperty(IDBDriverDefinitionConstants.URL_PROP_ID, driver.getProperty(IDBDriverDefinitionConstants.URL_PROP_ID));

			ProfileManager.getInstance().createProfile(DEFAULT_DS,	Messages.JBossRuntimeStartup_The_JBoss_AS_Hypersonic_embedded_database, HSQL_PROFILE_ID, props, "", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		
	}
	
	public void initializeDroolsRuntime(List<ServerDefinition> serverDefinitions) {
		List<DroolsRuntime> droolsRuntimes = new ArrayList<DroolsRuntime>();
		for (ServerDefinition serverDefinition : serverDefinitions) {
			String type = serverDefinition.getType();
			if (!droolsExists(serverDefinition)) {
				if (SOA_P.equals(type) || DROOLS.equals(type)) {
					File droolsRoot = serverDefinition.getLocation(); //$NON-NLS-1$
					if (droolsRoot.isDirectory()) {
						DroolsRuntime runtime = new DroolsRuntime();
						if (SOA_P.equals(type)) {
							runtime.setName("Drools - " + serverDefinition.getName()); //$NON-NLS-1$
						} else {
							runtime.setName("Drools " + serverDefinition.getVersion()+ " - " + serverDefinition.getName()); //$NON-NLS-1$
						}
						runtime.setPath(droolsRoot.getAbsolutePath());
						DroolsRuntimeManager.recognizeJars(runtime);
						runtime.setDefault(true);
						droolsRuntimes.add(runtime);
					}
				}
			}
		}
		if (droolsRuntimes.size() > 0) {
			DroolsRuntime[] dra = droolsRuntimes.toArray(new DroolsRuntime[0]);
			DroolsRuntimeManager.setDroolsRuntimes(dra);
		}
		
	}

	/**
	 * @param serverDefinition
	 * @return
	 */
	private boolean droolsExists(ServerDefinition serverDefinition) {
		DroolsRuntime[] droolsRuntimes = DroolsRuntimeManager.getDroolsRuntimes();
		for (DroolsRuntime dr:droolsRuntimes) {
			String location = dr.getPath();
			if (location != null && location.equals(serverDefinition.getLocation().getAbsolutePath())) {
				return true;
			}
		}
		return false;
	}

	public void initializeJbpmRuntime(List<ServerDefinition> serverDefinitions) {
		for (ServerDefinition serverDefinition : serverDefinitions) {
			if (!jbpmExists(serverDefinition)) {
				String type = serverDefinition.getType();
				if (SOA_P.equals(type)) {
					File jbpmRoot = new File(serverDefinition.getLocation(),"jbpm-jpdl"); //$NON-NLS-1$
					if (jbpmRoot.isDirectory()) {
						String version = JBossRuntimeLocator.JBPM3;
						if (JBossRuntimeLocator.isJbpm4(serverDefinition.getLocation().getAbsolutePath())) {
							version = JBossRuntimeLocator.JBPM4;
						}
						PreferencesManager.getInstance().initializeDefaultJbpmInstallation(serverDefinition.getName(), jbpmRoot.getAbsolutePath(), version);
					}
				} else if (JBPM.equals(type)) {
					PreferencesManager.getInstance().addJbpmInstallation(serverDefinition.getName(), serverDefinition.getLocation().getAbsolutePath(), serverDefinition.getVersion());
				}
			}
		}
		
	}

	/**
	 * @param serverDefinition
	 * @return
	 */
	private boolean jbpmExists(ServerDefinition serverDefinition) {
		Map<String, JbpmInstallation> jbpmMap = PreferencesManager.getInstance().getJbpmInstallationMap();
		Collection<JbpmInstallation> jbpmInstalations = jbpmMap.values();
		for (JbpmInstallation jbpm:jbpmInstalations) {
			String location = jbpm.location;
			if (location != null && location.equals(serverDefinition.getLocation().getAbsolutePath())) {
				return true;
			}
		}
		return false;
	}

}