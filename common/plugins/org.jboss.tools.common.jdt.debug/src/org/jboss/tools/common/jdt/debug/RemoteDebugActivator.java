/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.internal.runtime.InternalPlatform;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.internal.launching.SocketAttachConnector;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstallType;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.common.jdt.debug.sourcelookup.DebugLaunchConfigurationListener;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class RemoteDebugActivator implements BundleActivator, IPropertyChangeListener {
	
	public static final String UNKNOWN = "<Unknown>";
	public static final String LOCALHOST = "localhost"; //$NON-NLS-1$
	public static final String LAUNCH_CONFIGURATION_ID = "org.jboss.tools.common.jdt.debug.launching.JBossRemoteJavaApplication"; //$NON-NLS-1$
	public static final String REMOTE_JAVA_APPLICATION_ID = IJavaLaunchConfigurationConstants.ID_REMOTE_JAVA_APPLICATION;
	public static final String JBOSS_REMOTE_JAVA_APPLICATION = "JBossRemoteJavaApplication"; //$NON-NLS-1$
	public static final String JBOSS_SOURCE_PATH_COMPUTER_ID = "org.jboss.tools.common.jdt.debug.sourceLookup.remoteDebugSourcePathComputer"; //$NON-NLS-1$
	
	public static final String LAUNCH_CATEGORY = "org.eclipse.debug.ui.launchGroup.debug"; //$NON-NLS-1$
	public static final String DEFAULT_REMOTE_JBOSS_APP = "Remote myapp"; //$NON-NLS-1$
	public static final String JBOSS_TEMP_JAVA_APPLICATION = "jbossTempJavaApplication"; //$NON-NLS-1$
	public static final String DT_SOCKET = "dt_socket"; //$NON-NLS-1$
	public static final String SET_AS_DEFAULT = "setAsDefault"; //$NON-NLS-1$
	public static final String ATTR_SELECTED_PROJECTS = "selectedProjects";  //$NON-NLS-1$
	public static final String MAVEN_SOURCEPATH_PROVIDER = "org.eclipse.m2e.launchconfig.sourcepathProvider"; //$NON-NLS-1$
	public static final String MAVEN_CLASSPATH_PROVIDER = "org.eclipse.m2e.launchconfig.classpathProvider"; //$NON-NLS-1$
	public static final String JDT_JAVA_APPLICATION = "org.eclipse.jdt.launching.localJavaApplication"; //$NON-NLS-1$
	public static final String JDT_JUNIT_TEST = "org.eclipse.jdt.junit.launchconfig"; //$NON-NLS-1$

	private static final String JAVA_HOME = "java.home"; //$NON-NLS-1$
	private static final String TOOLS_JAR = File.separator + "lib" + File.separator + "tools.jar"; //$NON-NLS-1$ //$NON-NLS-2$
	private static final String PLUGIN_ID = "org.jboss.tools.common.jdt.debug"; //$NON-NLS-1$
	
	private static String[] WIN_CMD_ARRAY = { "netstat", "-ona", "-p", "tcp" };
	private static String[] MAC_CMD_ARRAY = { "lsof", "-i", "-P" };
	private static String[] LINUX_CMD_ARRAY = { "netstat", "-nlt", "-p", "" };
	
	private static final String MAVEN_PLUGIN_ID = "org.eclipse.m2e.core"; //$NON-NLS-1$
	public static final String MAVEN_NATURE = MAVEN_PLUGIN_ID + ".maven2Nature"; //$NON-NLS-1$
	
	private boolean jdk = false;
	private boolean logged = false;
	private ClassLoader toolsLoader;
	private static RemoteDebugActivator plugin;

	private static BundleContext context;
	
	private DebugLaunchConfigurationListener listener;

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		RemoteDebugActivator.context = bundleContext;
		plugin = this;
		JavaRuntime.getPreferences().addPropertyChangeListener(this);
		listener= new DebugLaunchConfigurationListener();
		DebugPlugin.getDefault().getLaunchManager().addLaunchConfigurationListener(listener);
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		RemoteDebugActivator.context = null;
		JavaRuntime.getPreferences().removePropertyChangeListener(this);
		DebugPlugin.getDefault().getLaunchManager().removeLaunchConfigurationListener(listener);
	    
	}
	
	public static RemoteDebugActivator getDefault() {
		return plugin;
	}
	
	private File getToolsJar() {
		String javaHome = System.getProperty(JAVA_HOME);
		File javaHomeFile = new File(javaHome);
		File tools = getFile(javaHomeFile);
		if (tools.exists()) {
			return tools;
		}
		File parentDir = new File(javaHome + File.separator + ".."); //$NON-NLS-1$
		tools = getFile(parentDir);
		if (tools.exists()) {
			return tools;
		}
		if (parentDir.isDirectory()) {
            for (File child : parentDir.listFiles()) {
                if (!child.isDirectory()) {
                    continue;
                }
                tools = getFile(child);
        		if (tools.exists()) {
        			return tools;
        		}
            }
        }
        for (IVMInstallType type : JavaRuntime.getVMInstallTypes()) {
            for (IVMInstall install : type.getVMInstalls()) {
                File jdkRoot = install.getInstallLocation();
                tools = getFile(jdkRoot);
        		if (tools.exists()) {
        			return tools;
        		}
            }
        }

        return null;
    }

	private File getFile(File dir) {
		File tools = new File(dir, TOOLS_JAR);
		if (tools.exists()) {
			return tools;
		}
		return tools;
	}

	private ClassLoader getToolsLoader() {
		if (toolsLoader == null) {
			toolsLoader = Thread.currentThread().getContextClassLoader();
			File toolsJar = getToolsJar();
			if (toolsJar == null) {
				return toolsLoader;
			}
			try {
				toolsJar = toolsJar.getCanonicalFile();
			} catch (IOException e1) {
				return toolsLoader;
			}
			if (toolsJar.exists()) {
				URL toolsUrl;
				try {
					toolsUrl = toolsJar.toURI().toURL();
				} catch (MalformedURLException e) {
					return toolsLoader;
				}
				toolsLoader = new URLClassLoader(new URL[] { toolsUrl },
						toolsLoader);
			}
		}
		return toolsLoader;
	}
	
	private List<VmModel> getVmModels(String name, IProgressMonitor monitor) {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		List<VmModel> models = new ArrayList<VmModel>();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> hostClazz = toolsLoader.loadClass("sun.jvmstat.monitor.MonitoredHost"); //$NON-NLS-1$
            Method method = hostClazz.getDeclaredMethod("getMonitoredHost", //$NON-NLS-1$
                    new Class[] { String.class });
            Object host =  method.invoke(null, name);
            if (host != null) {
            	method = hostClazz.getDeclaredMethod("activeVms", new Class[0]); //$NON-NLS-1$
            	Object vmObjects = method.invoke(host, new Object[0]);
            	int i = 1;
            	if (vmObjects instanceof Set) {
            		Set<Integer> vmPids = (Set<Integer>) vmObjects;
            		int size = vmPids.size();
            		monitor.beginTask("Discovering Remote Aplications", vmPids.size() + 1);
            		for (Integer vmPid : vmPids) {
            			monitor.worked(1);
            			monitor.setTaskName(i++ + " out of " + size + ": Discovering port, main class and arguments for process id " + vmPid);
            			VmModel model = getVmModelsUsingTools(hostClazz, host, vmPid, monitor);
            			if (model != null) {
            				models.add(model);
            			} else {
            				model = getVmModelUsingOsCommand(vmPid, monitor);
            				if (model != null) {
            					models.add(model);
            				}
            			}
            		}
            	}
            }
            jdk = true;
		} catch (ClassNotFoundException e) {
			logWarning(e);
			jdk = false;
		} catch (SecurityException e) {
			logWarning(e);
		} catch (NoSuchMethodException e) {
			logWarning(e);
		} catch (IllegalArgumentException e) {
			logWarning(e);
		} catch (IllegalAccessException e) {
			logWarning(e);
		} catch (InvocationTargetException e) {
			logWarning(e);
		}  finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
		return models;
	}

	private String[] getOsCommand(String pid) {
		if (Platform.OS_WIN32.equals(Platform.getOS())) {
			return WIN_CMD_ARRAY;
		} else if (Platform.OS_MACOSX.equals(Platform.getOS())) {
			return MAC_CMD_ARRAY;
		} else if (Platform.OS_LINUX.equals(Platform.getOS())) {
			String[] cmdarray = LINUX_CMD_ARRAY;
			cmdarray[cmdarray.length-1] = pid;
			return cmdarray;
		}
		return null;
	}
	
	private VmModel getVmModelUsingOsCommand(Integer vmPid, IProgressMonitor monitor) {
		if (vmPid == null) {
			return null;
		}
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		if (monitor.isCanceled()) {
			return null;
		}
		String vmPidStr = String.valueOf(vmPid);
		String[] cmdarray = getOsCommand(vmPidStr);
		if (cmdarray == null) {
			return null;
		}
		InputStream is = null;
		BufferedReader reader = null;
		try {
			Process process = Runtime.getRuntime().exec(cmdarray);
			if (monitor.isCanceled()) {
				return null;
			}
			is = process.getInputStream();
			reader = new BufferedReader(new InputStreamReader(is));
			String line = reader.readLine();
			
			while (line != null) {
				if (line.contains("LISTEN")) {
					line = clearWhiteSpace(line);
					VmModel model = processLine(line, vmPidStr);
					if (model != null) {
						return model;
					}
				}
				if (monitor.isCanceled()) {
					return null;
				}
				line = reader.readLine();
			}
		} catch (IOException e) {
			logWarning(e);
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (IOException e1) {
					// ignore
				}
			}
			if (is != null) {
				try {
					is.close();
				} catch (IOException e1) {
					// ignore
				}
			}
		}

		return null;
	}

	private VmModel processLine(String line, String vmPidStr) {
		if (Platform.OS_WIN32.equals(Platform.getOS())) {
			return processWinLine(line, vmPidStr);
		} else if (Platform.OS_MACOSX.equals(Platform.getOS())) {
			return processMacLine(line, vmPidStr);
		} else if (Platform.OS_LINUX.equals(Platform.getOS())) {
			return processLinuxLine(line, vmPidStr);
		}
		return null;
	}

	private VmModel processLinuxLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 7);
		if (elements.length >= 7 &&
				elements[0] != null && elements[0].equals("tcp")) {
			if (elements[6] != null) {
				String[] pids = elements[6].split("/");
				String pid;
				if (pids.length == 2) {
					pid = pids[0];
					if (pid != null && pid.equals(vmPidStr)) {
						if (elements[3] != null) {
							String port = getPort(elements[3]);
							if (port != null) {
								VmModel model = new VmModel();
								model.setPid(vmPidStr);
								model.setMainClass(UNKNOWN);
								model.setPort(port);
								model.setTransport(DT_SOCKET);
								return model;
							}
						}
					}
				}
			}
		}
		return null;
	}

	private VmModel processMacLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 10);
		if (elements.length >= 10 &&
				elements[1] != null && elements[1].equals(vmPidStr) && 
				elements[7] != null && elements[7].equals("TCP")) {
			if (elements[8] != null) {
				String port = getPort(elements[8]);
				if (port != null && !port.isEmpty()) {
					VmModel model = new VmModel();
					model.setPid(vmPidStr);
					model.setMainClass(UNKNOWN);
					model.setPort(port);
					model.setTransport(DT_SOCKET);
					return model;
				}
			}
		}
		return null;
	}

	private String getPort(String element) {
		String[] ports = element.split(":");
		String port;
		if (ports.length == 2) {
			port = ports[1];
		} else {
			port = ports[0];
		}
		return port;
	}

	private VmModel processWinLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 5);
		if (elements.length == 5 && elements[4] != null && elements[4].equals(vmPidStr)) {
			if (elements[1] != null) {
				String port = getPort(elements[1]);
				if (port != null && !port.isEmpty()) {
					VmModel model = new VmModel();
					model.setPid(vmPidStr);
					model.setMainClass(UNKNOWN);
					model.setPort(port);
					model.setTransport(DT_SOCKET);
					return model;
				}
			}
		}
		return null;
	}

	private String clearWhiteSpace(String line) {
		line = line.trim();
		while (line.contains("\t")) {
			line = line.replace(" ", "\t");
		}
		while (line.contains("  ")) {
			line = line.replace("  ", " ");
		}
		return line;
	}

	private VmModel getVmModelsUsingTools(Class<?> hostClazz, Object host, Integer vmPid, IProgressMonitor monitor) {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		if (monitor.isCanceled()) {
			return null;
		}
		String vmPidStr = String.valueOf(vmPid);
		try {
			Class<?> vmIdentifier = toolsLoader.loadClass("sun.jvmstat.monitor.VmIdentifier"); //$NON-NLS-1$
			Constructor<?> vmIdentifierConstructor = vmIdentifier.getConstructor(new Class[] {String.class});
			Object vmIdentifierObject = vmIdentifierConstructor.newInstance(new Object[] {vmPidStr});
			Method getMonitoredVm = hostClazz.getDeclaredMethod("getMonitoredVm", new Class[] {vmIdentifier}); //$NON-NLS-1$
			Object vm = getMonitoredVm.invoke(host, vmIdentifierObject);
			Class<?> vmUtil = toolsLoader.loadClass("sun.jvmstat.monitor.MonitoredVmUtil"); //$NON-NLS-1$
			Class<?> monitoredVm = toolsLoader.loadClass("sun.jvmstat.monitor.MonitoredVm"); //$NON-NLS-1$
			if (monitor.isCanceled()) {
				return null;
			}
			VmModel model = new VmModel();
			model.setPid(vmPidStr);
			Method jvmArgs = vmUtil.getDeclaredMethod("jvmArgs", //$NON-NLS-1$
					new Class[] { monitoredVm });
			Object jvmArgsObj = jvmArgs.invoke(null, vm);
			if (jvmArgsObj instanceof String) {
				model.setJvmArgs((String) jvmArgsObj);
			}
			Method mainClass = vmUtil.getDeclaredMethod(
					"mainClass", new Class[] { monitoredVm, //$NON-NLS-1$
							boolean.class });
			Object mainClassObj = mainClass.invoke(null,
					new Object[] { vm, true });
			if (mainClassObj instanceof String) {
				model.setMainClass((String) mainClassObj);
			}
			Method mainArgs = vmUtil.getDeclaredMethod("mainArgs", //$NON-NLS-1$
					new Class[] { monitoredVm });
			Object mainArgsObj = mainArgs.invoke(null, vm);
			if (mainArgsObj instanceof String) {
				model.setMainArgs((String) mainArgsObj);
			}
			return model;
		} catch (Exception e) {
			logWarning(e);
		}
		return null;
	}

	private void logWarning(Exception e) {
		if (!logged) {
			IStatus status = new Status(IStatus.WARNING, context.getBundle().getSymbolicName(), e.getLocalizedMessage(), e);
			getLog().log(status);
			logged = true;
		}
	}
	
	public ILog getLog() {
		Bundle bundle = context.getBundle();
		return InternalPlatform.getDefault().getLog(bundle);
	}
	
	public VmModel[] getDebugModels(String name, IProgressMonitor monitor) {
		List<VmModel> debugModels = new ArrayList<VmModel>();
		List<VmModel> models = getVmModels(name, monitor);
		if (models == null) {
			return null;
		}
		for (VmModel model:models) {
			if (model.getPort() != null && DT_SOCKET.equals(model.getTransport())) {
				debugModels.add(model);
			}
		}
		return debugModels.toArray(new VmModel[0]);
	}

	public boolean isJdk() {
		if (!jdk && toolsLoader == null) {
			getVmModels(LOCALHOST, new NullProgressMonitor());
		}
		return jdk;
	}
	
	public static void log(Exception e, String message) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
		plugin.getLog().log(status);
	}

	public static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e
				.getLocalizedMessage(), e);
		plugin.getLog().log(status);
	}

	public static IVMConnector getDefaultVMConnector() {
		IVMConnector[] connectors = JavaRuntime.getVMConnectors();
		for (IVMConnector connector:connectors) {
			if (connector instanceof SocketAttachConnector) {
				return connector;
			}
		}
		return null;
	}

	public VmModel[] getDebugModels(IProgressMonitor monitor) {
		return getDebugModels(LOCALHOST, monitor);
	}
	
	public ILaunchConfiguration getDefaultLaunchConfiguration() {
		ILaunchConfiguration[] configs = getLaunchConfigurations();
		if (configs != null && configs.length > 0) {
			for (ILaunchConfiguration config:configs) {
				boolean isDefault = false;
				try {
					isDefault = config.getAttribute(SET_AS_DEFAULT, false);
				} catch (CoreException e) {
					// ignore
				}
				if (isDefault) {
					return config;
				}
			}
			if (configs.length == 1) {
				try {
					ILaunchConfigurationWorkingCopy wc = configs[0].getWorkingCopy();
					wc.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, true);
					wc.doSave();
				} catch (CoreException e) {
					log(e);
				}
			}
			return configs[0];
		}
		return null;
	}

	public ILaunchConfiguration[] getLaunchConfigurations() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		try {
			ILaunchConfiguration[] configs = manager.getLaunchConfigurations(type);
			List<ILaunchConfiguration> jbossConfigurations = new ArrayList<ILaunchConfiguration>();
			for (ILaunchConfiguration config:configs) {
				if (config.getAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false)) {
					jbossConfigurations.add(config);
				}
			}
			return jbossConfigurations.toArray(new ILaunchConfiguration[0]);
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		String property = event.getProperty();
		if (JavaRuntime.PREF_VM_XML.equals(property)) {
			toolsLoader = null;
			logged = false;
		}
	}
	
	public static boolean m2eExists() {
		Bundle bundle = Platform.getBundle(MAVEN_PLUGIN_ID);
		return bundle != null;
	}
	
	public static ILaunchConfiguration createTemporaryLaunchConfiguration(String projectName, String typeId)
			throws CoreException {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(typeId);
		ILaunchConfigurationWorkingCopy wc = type.newInstance(null, manager.generateLaunchConfigurationName(RemoteDebugActivator.JBOSS_TEMP_JAVA_APPLICATION));
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, projectName);
		ILaunchConfiguration config = wc.doSave();
		return config;
	}
	
	public static ILaunchConfigurationWorkingCopy createNewLaunchConfiguration(ILaunchConfigurationType type)
			throws CoreException {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationWorkingCopy wc = type.newInstance(null, manager.generateLaunchConfigurationName(RemoteDebugActivator.DEFAULT_REMOTE_JBOSS_APP));
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_ALLOW_TERMINATE, false);
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_CONNECTOR, RemoteDebugActivator.getDefaultVMConnector().getIdentifier());
		wc.setAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, true);
		return wc;
	}
}
