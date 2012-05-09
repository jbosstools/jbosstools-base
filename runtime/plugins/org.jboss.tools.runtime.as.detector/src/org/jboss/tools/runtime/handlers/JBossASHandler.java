/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime.handlers;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
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
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeType;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerType;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerCore;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerDelegate;
import org.jboss.ide.eclipse.as.core.publishers.LocalPublishMethod;
import org.jboss.ide.eclipse.as.core.server.IDeployableServer;
import org.jboss.ide.eclipse.as.core.server.bean.JBossServerType;
import org.jboss.ide.eclipse.as.core.server.bean.ServerBean;
import org.jboss.ide.eclipse.as.core.server.bean.ServerBeanLoader;
import org.jboss.ide.eclipse.as.core.util.IJBossToolingConstants;
import org.jboss.tools.runtime.as.detector.IJBossRuntimePluginConstants;
import org.jboss.tools.runtime.as.detector.Messages;
import org.jboss.tools.runtime.as.detector.RuntimeAsActivator;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.osgi.framework.Bundle;

public class JBossASHandler extends AbstractRuntimeDetector implements IJBossRuntimePluginConstants {
	
	private static String[] hasIncludedRuntimes = new String[] {SOA_P, EAP, EPP, EWP, SOA_P_STD};
	private static final String DROOLS = "DROOLS"; // NON-NLS-1$
	private static final String ESB = "ESB"; //$NON-NLS-1$
	
	// This constants are made to avoid dependency with org.jboss.ide.eclipse.as.core plugin
	public static final String RUNTIME_TYPES[] = IJBossToolingConstants.ALL_JBOSS_RUNTIMES;
	public static final String SERVER_TYPES[] = IJBossToolingConstants.ALL_JBOSS_SERVERS;
	
	public static final HashMap<String,String> SERVER_DEFAULT_NAME = new HashMap<String, String>();
	static {
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_32, Messages.JBossRuntimeStartup_JBoss_Application_Server_3_2);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_40, Messages.JBossRuntimeStartup_JBoss_Application_Server_4_0);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_42, Messages.JBossRuntimeStartup_JBoss_Application_Server_4_2);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_50, Messages.JBossRuntimeStartup_JBoss_Application_Server_5_0);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_51, Messages.JBossRuntimeStartup_JBoss_Application_Server_5_1);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_60, Messages.JBossRuntimeStartup_JBoss_Application_Server_6_0);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_EAP_43, Messages.JBossRuntimeStartup_JBoss_EAP_Server_4_3);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_EAP_50, Messages.JBossRuntimeStartup_JBoss_EAP_Server_5_0);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_70, Messages.JBossRuntimeStartup_JBoss_Application_Server_7_0);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_AS_71, Messages.JBossRuntimeStartup_JBoss_Application_Server_7_1);
		SERVER_DEFAULT_NAME.put(IJBossToolingConstants.SERVER_EAP_60, Messages.JBossRuntimeStartup_JBoss_EAP_Server_6_0);
	}

	public void initializeRuntimes(List<RuntimeDefinition> serverDefinitions) {
		createJBossServerFromDefinitions(serverDefinitions);
	}
		
	private static File getLocation(RuntimeDefinition serverDefinition) {
		String type = serverDefinition.getType();
		String version = serverDefinition.getVersion();
		if (EAP.equals(type) && version != null && version.startsWith("6") ) {
			return serverDefinition.getLocation();
		}
		if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type)) {
			return new File(serverDefinition.getLocation(), "jboss-as");
		}
		if (SOA_P_STD.equals(type)) {
			return new File(serverDefinition.getLocation(),"jboss-esb"); //$NON-NLS-1$					
		}
		if(EWP.equals(type)) {
				return new File(serverDefinition.getLocation(),"jboss-as-web"); //$NON-NLS-1$
		}
		if (AS.equals(type) || EAP_STD.equals(type)) {
			return serverDefinition.getLocation();
		}
		return null;
	}
	
	public static void createJBossServerFromDefinitions(List<RuntimeDefinition> serverDefinitions) {
		for (RuntimeDefinition serverDefinition:serverDefinitions) {
			if (serverDefinition.isEnabled()) {
				File asLocation = getLocation(serverDefinition);
				if (asLocation == null || !asLocation.isDirectory()) {
					continue;
				}
				String type = serverDefinition.getType();
				if (SOA_P.equals(type) || EAP.equals(type) || EPP.equals(type)
						|| SOA_P_STD.equals(type) || EWP.equals(type)
						|| EAP_STD.equals(type) || AS.equals(type)) {
					String typeId = new ServerBeanLoader(asLocation).getServerAdapterId();
					String name = serverDefinition.getName();
					String runtimeName = name + " " + RUNTIME; //$NON-NLS-1$
					createJBossServer(asLocation, typeId, name, runtimeName);
				}
			}
			createJBossServerFromDefinitions(serverDefinition.getIncludedServerDefinitions());
		}	
	}

	private static boolean serverExistsForPath(IPath locPath) {
		IServer[] servers = ServerCore.getServers();
		for (int i = 0; i < servers.length; i++) {
			IRuntime runtime = servers[i].getRuntime();
			if(runtime != null && runtime.getLocation() != null && runtime.getLocation().equals(locPath)) {
				return true;
			}
		}
		return false;
	}
	
	private static IRuntime findRuntimeForPath(IPath locPath) {
		IRuntime[] runtimes = ServerCore.getRuntimes();
		for (int i = 0; i < runtimes.length; i++) {
			if (runtimes[i] == null || runtimes[i].getLocation() == null) {
				continue;
			}
			if (runtimes[i].getLocation().equals(locPath)) {
				return runtimes[i];
			}
		}
		return null;
	}
	
	private static void createJBossServer(File asLocation, String serverTypeId, String name, String runtimeName) {
		if (asLocation == null || !asLocation.isDirectory() || serverTypeId == null)
			return;
		IServerType serverType = ServerCore.findServerType(serverTypeId);
		if( serverType == null )
			return;
		IRuntimeType rtType = serverType.getRuntimeType();
		if( rtType == null )
			return;
		
		IPath jbossAsLocationPath = new Path(asLocation.getAbsolutePath());
		if( serverExistsForPath(jbossAsLocationPath))
			return;
		
		IRuntime runtime = findRuntimeForPath(jbossAsLocationPath);
		IProgressMonitor progressMonitor = new NullProgressMonitor();
		try {
			if (runtime == null) {
				runtime = createRuntime(runtimeName, asLocation.getAbsolutePath(), progressMonitor, rtType);
			}
			if (runtime != null) {
				createServer(progressMonitor, runtime, serverType, name);
			}

			new DriverUtility().createDriver(asLocation.getAbsolutePath(), serverType);
		} catch (CoreException e) {
			RuntimeAsActivator.log(e,Messages.JBossRuntimeStartup_Cannot_create_new_JBoss_Server);
		} catch (ConnectionProfileException e) {
			RuntimeAsActivator.log(e,Messages.JBossRuntimeStartup_Cannott_create_new_DTP_Connection_Profile);
		}
	}

	/**
	 * Creates new JBoss AS Runtime
	 * @param jbossASLocation location of JBoss AS
	 * @param progressMonitor
	 * @return runtime working copy
	 * @throws CoreException
	 */
	private static IRuntime createRuntime(String runtimeName, String jbossASLocation, 
			IProgressMonitor progressMonitor, IRuntimeType rtType) throws CoreException {
		IRuntimeWorkingCopy runtime = null;
		IPath jbossAsLocationPath = new Path(jbossASLocation);
		runtime = rtType.createRuntime(null, progressMonitor);
		runtime.setLocation(jbossAsLocationPath);
		if(runtimeName!=null) {
			runtime.setName(runtimeName);				
		}
		return runtime.save(false, progressMonitor);
	}

	/**
	 * Creates new JBoss Server
	 * @param progressMonitor
	 * @param runtime parent JBoss AS Runtime
	 * @return server working copy
	 * @throws CoreException
	 */
	private static void createServer(IProgressMonitor progressMonitor, IRuntime runtime,
			IServerType serverType, String name) throws CoreException {
		if (name == null)
			name = SERVER_DEFAULT_NAME.get(serverType.getId());
		if( !serverWithNameExists(name)) {
			createServer2(runtime, serverType, name, LocalPublishMethod.LOCAL_PUBLISH_METHOD);
		}
	}
	
	private static boolean serverWithNameExists(String name) {
		IServer[] servers = ServerCore.getServers();
		for (IServer server:servers) {
			if (name.equals(server.getName()) ) {
				return true;
			}
		}
		return false;
	}
	
	public static IServer createServer2(IRuntime currentRuntime, IServerType serverType, String serverName, String mode) throws CoreException {
		IServerWorkingCopy serverWC = serverType.createServer(null, null,
				new NullProgressMonitor());
		serverWC.setRuntime(currentRuntime);
		serverWC.setName(serverName);
		serverWC.setServerConfiguration(null);
		serverWC.setAttribute(IDeployableServer.SERVER_MODE, mode); 
		return serverWC.save(true, new NullProgressMonitor());
	}

	public RuntimeDefinition getServerDefinition(File root,
			IProgressMonitor monitor) {
		if (monitor.isCanceled() || root == null || !isEnabled()) {
			return null;
		}
		ServerBeanLoader loader = new ServerBeanLoader(root);
		ServerBean serverBean = loader.getServerBean();
		
		if (!JBossServerType.UNKNOWN.equals(serverBean.getType())) {
			RuntimeDefinition serverDefinition = new RuntimeDefinition(serverBean.getName(), 
					serverBean.getVersion(), serverBean.getType().getId(), new File(serverBean.getLocation()));
			calculateIncludedServerDefinition(serverDefinition, monitor);
			return serverDefinition;
		}
		return null;
	}
	
	private void calculateIncludedServerDefinition(
			RuntimeDefinition serverDefinition, IProgressMonitor monitor) {
		if (serverDefinition == null || serverDefinition.getType() == null) {
			return;
		}
		String type = serverDefinition.getType();
		if (!hasIncludedRuntimes(type)) {
			return;
		}
		serverDefinition.getIncludedServerDefinitions().clear();
		List<RuntimeDefinition> serverDefinitions = serverDefinition
				.getIncludedServerDefinitions();
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		final File location = getLocation(serverDefinition);
		File[] directories = serverDefinition.getLocation().listFiles(
				new FileFilter() {

					@Override
					public boolean accept(File file) {
						if (!file.isDirectory() || file.equals(location)) {
							return false;
						}
						return true;
					}
				});
		boolean saved = isEnabled();
		try {
			setEnabled(false);
			for (File directory : directories) {
				List<RuntimeDefinition> definitions = new ArrayList<RuntimeDefinition>();
				locator.searchDirectory(directory, definitions, 1, monitor);
				for (RuntimeDefinition definition:definitions) {
					definition.setParent(serverDefinition);
				}
				serverDefinitions.addAll(definitions);
			}
			if (SOA_P.equals(type) || SOA_P_STD.equals(type)) {
				addDrools(serverDefinition);
				addEsb(serverDefinition);
			}
		} finally {
			setEnabled(saved);
		}
	}

	private void addDrools(RuntimeDefinition serverDefinition) {
		if (serverDefinition == null) {
			return;
		}
		Bundle drools = Platform.getBundle("org.drools.eclipse");
		Bundle droolsDetector = Platform
				.getBundle("org.jboss.tools.runtime.drools.detector");
		if (drools != null && droolsDetector != null) {
			File droolsRoot = serverDefinition.getLocation();
			if (droolsRoot.isDirectory()) {
				String name = "Drools - " + serverDefinition.getName();
				RuntimeDefinition droolsDefinition = new RuntimeDefinition(
						name, serverDefinition.getVersion(), DROOLS,
						droolsRoot);
				droolsDefinition.setParent(serverDefinition);
				serverDefinition.getIncludedServerDefinitions().add(
						droolsDefinition);
			}
		}
	}
	
	private void addEsb(RuntimeDefinition serverDefinition) {
		if (serverDefinition == null) {
			return;
		}
		Bundle esb = Platform.getBundle("org.jboss.tools.esb.project.core");
		Bundle esbDetectorPlugin = Platform
				.getBundle("org.jboss.tools.runtime.esb.detector");
		if (esb != null && esbDetectorPlugin != null) {
			String type = serverDefinition.getType();
			File esbRoot;
			if (SOA_P.equals(type)) {
				esbRoot = serverDefinition.getLocation();
			} else {
				esbRoot = new File(serverDefinition.getLocation(), "jboss-esb"); //$NON-NLS-1$
			}
			if (esbRoot.isDirectory()) {
				String name = "ESB - " + serverDefinition.getName();
				String version="";
				RuntimeDefinition esbDefinition = new RuntimeDefinition(
						name, version, ESB,
						esbRoot);
				IRuntimeDetector esbDetector = RuntimeCoreActivator.getEsbDetector();
				if (esbDetector != null) {
					version = esbDetector.getVersion(esbDefinition);
					esbDefinition.setVersion(version);
				}
				
				esbDefinition.setParent(serverDefinition);
				serverDefinition.getIncludedServerDefinitions().add(
						esbDefinition);
			}
		}
	}

	private boolean hasIncludedRuntimes(String type) {
		for (String t:hasIncludedRuntimes) {
			if (t.equals(type)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean exists(RuntimeDefinition serverDefinition) {
		if (serverDefinition == null || serverDefinition.getLocation() == null) {
			return false;
		}
		File location = getLocation(serverDefinition);
		if (location == null || !location.isDirectory()) {
			return false;
		}
		String path = location.getAbsolutePath();
		if (path == null) {
			return false;
		}
		IServer[] servers = ServerCore.getServers();
		for (int i = 0; i < servers.length; i++) {
			IRuntime runtime = servers[i].getRuntime();
			if (runtime == null || runtime.getLocation() == null) {
				continue;
			}
			if(path.equals(runtime.getLocation().toOSString())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void computeIncludedServerDefinition(
			RuntimeDefinition serverDefinition) {
		if (serverDefinition == null) {
			return;
		}
		String type = serverDefinition.getType();
		if (AS.equals(type)) {
			return;
		}
		calculateIncludedServerDefinition(serverDefinition, new NullProgressMonitor());
	}
	
}
