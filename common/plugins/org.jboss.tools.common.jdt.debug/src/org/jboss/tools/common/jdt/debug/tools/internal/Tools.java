/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.tools.internal;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jdt.launching.IVMInstallType;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.common.jdt.debug.IPropertyKeys;
import org.jboss.tools.common.jdt.debug.Messages;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.tools.ToolsCoreException;

/**
 * The class enabling to invoke the APIs in <tt>tools.jar</tt>.
 */
public class Tools implements IPreferenceChangeListener, IToolsConstants, IPropertyChangeListener  {

	/** The local host name. */
	static final String LOCALHOST = "localhost"; //$NON-NLS-1$

	private ClassLoader toolsLoader;

	/** The shared instance of this class. */
	private static Tools tools;

	/** The state indicating if ready to use. */
	private boolean isReady;
	
	private File toolsJarFile;

	/**
	 * Gets the shared instance of this class.
	 * 
	 * @return The shared instance
	 */
	public static synchronized Tools getInstance() {
		if (tools == null) {
			tools = new Tools();
		}
		return tools;
	}

	/**
	 * The constructor.
	 */
	private Tools() {
		reset();
		if (!isReady) {
			IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(RemoteDebugActivator.PLUGIN_ID);
			prefs.addPreferenceChangeListener(this);
		}
		JavaRuntime.getPreferences().addPropertyChangeListener(this);
	}
	

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		String property = event.getProperty();
		if (JavaRuntime.PREF_VM_XML.equals(property)) {
			reset();
		}
	}
	
	@Override
	public void preferenceChange(PreferenceChangeEvent event) {
		if (IPropertyKeys.JDK_VM_INSTALL.equals(event.getKey())) {
			if (!isReady) {
				reset();
			}
		}
	}
	
	public File getToolsJarFile() {
		return toolsJarFile;
	}

	public void reset() {
		toolsLoader = null;
		getToolsLoader();
		isReady = validateClassPathAndLibraryPath();
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
				toolsJarFile = toolsJar;
				URL toolsUrl;
				try {
					toolsUrl = toolsJar.toURI().toURL();
				} catch (MalformedURLException e) {
					return toolsLoader;
				}
				toolsLoader = new URLClassLoader(new URL[] { toolsUrl }, toolsLoader);
			}
		}
		return toolsLoader;
	}

	private File getToolsJar() {
		// Find the jdk root for the currently running java home
		String jdkRootDirectory = findHomeDirectoryToAddToClasspath();
		File file = new File(jdkRootDirectory + TOOLS_JAR);
		return file;
	}

	/**
	 * Gets the state indicating if it is ready to use.
	 * 
	 * @return <tt>true</tt> if it is ready to use
	 */
	public boolean isReady() {
		return isReady;
	}

	/**
	 * Validates the JDK root directory.
	 * 
	 * @param jdkRootDirectory
	 *            The JDK root directory
	 * @return The error message or <tt>null</tt> if not found
	 */
	public String validateJdkRootDirectory(String jdkRootDirectory) {
		if (jdkRootDirectory == null || jdkRootDirectory.trim().isEmpty()) {
			return Messages.NoJdkDirectoryFoundMsg;
		}
		// check if directory exists
		File directory = new File(jdkRootDirectory);
		if (!directory.exists() || !directory.isDirectory()) {
			return Messages.directoryNotExistMsg;
		}

		// check if tools.jar exists
		File toolsJarFile = new File(jdkRootDirectory + TOOLS_JAR);
		if (!toolsJarFile.exists()) {
			return Messages.notJdkRootDirectoryMsg;
		}

		// checks if "attach" shared library exist
		String libraryPath = getJreLibraryPath(jdkRootDirectory);
		if (libraryPath == null) {
			return Messages.notJdkRootDirectoryMsg;
		}
		return null;
	}

	private IVMInstall[] getAllVMInstalls() {
		ArrayList<IVMInstall> all = new ArrayList<IVMInstall>();
		for (IVMInstallType type : JavaRuntime.getVMInstallTypes()) {
			for (IVMInstall install : type.getVMInstalls()) {
				all.add(install);
			}
		}
		return (IVMInstall[]) all.toArray(new IVMInstall[all.size()]);
	}

	private int compareJavaVersions(String version0, String version1) {
		int[] arg0majorMinor = getMajorMinor(version0);
		int[] arg1majorMinor = getMajorMinor(version1);
		if (arg0majorMinor[0] < arg1majorMinor[0])
			return -1;
		if (arg0majorMinor[0] > arg1majorMinor[0])
			return 1;
		if (arg0majorMinor[1] < arg1majorMinor[1])
			return -1;
		if (arg0majorMinor[1] > arg1majorMinor[1])
			return 1;
		return 0;
	}

	/**
	 * Get all IVMInstalls that have a major.minor <= the currently running
	 * jre's major.minor
	 * 
	 * @return
	 */
	public IVMInstall[] getAllCompatibleInstalls() {
		String currentVersion = System.getProperty("java.version");
		ArrayList<IVMInstall2> compat = new ArrayList<IVMInstall2>();
		IVMInstall[] all = getAllVMInstalls();
		for (int i = 0; i < all.length; i++) {
			if (all[i] instanceof IVMInstall2) {
				String vers = ((IVMInstall2) all[i]).getJavaVersion();
				// if running jre is greater than or equal the vm install, 
				// it's probably compatible
				if (compareJavaVersions(currentVersion, vers) >= 0) {
					compat.add((IVMInstall2) all[i]);
				}
			}
		}

		// Sort them by version number, so higher matches are at the beginning
		// of the list
		Comparator<IVMInstall2> comparator = new Comparator<IVMInstall2>() {
			public int compare(IVMInstall2 arg0, IVMInstall2 arg1) {
				String arg0vers = arg0.getJavaVersion();
				String arg1vers = arg1.getJavaVersion();
				return compareJavaVersions(arg0vers, arg1vers);
			}
		};

		Collections.sort(compat, comparator);
		Collections.reverse(compat);
		return (IVMInstall[]) compat.toArray(new IVMInstall[compat.size()]);
	}

	private int[] getMajorMinor(String version) {
		Matcher m = Pattern.compile("^(\\d+)\\.(\\d+)\\..*").matcher(version);
		if (!m.matches()) {
			throw new IllegalArgumentException("Malformed version string");
		}
		return new int[] { Integer.parseInt(m.group(1)), // major
				Integer.parseInt(m.group(2)) };
	}

	
	private boolean loadingFirstValidVM = false;
	
	/**
	 * Find the first vm-install that is valid
	 * 
	 * @return The JDK root directory, or empty string if not found
	 */
	private IVMInstall findFirstValidVMInstall() {
		// search from the JREs that are compatible based on java version
		IVMInstall[] all = getAllCompatibleInstalls();
		for (int i = 0; i < all.length; i++) {
			String jdkRootDirectory = all[i].getInstallLocation().getPath();
			if (null == validateJdkRootDirectory(jdkRootDirectory)) {
//				RemoteDebugActivator.pluginLog().logMessage(IStatus.INFO,
//						NLS.bind(Messages.jdkRootDirectoryFoundMsg, jdkRootDirectory), new Exception());
				return all[i];
			}
		}
		RemoteDebugActivator.pluginLog().logMessage(IStatus.WARNING, Messages.jdkRootDirectoryNotFoundMsg,
				new Exception(Messages.jdkRootDirectoryNotFoundMsg));
		return null; //$NON-NLS-1$
	}

	public String findJdkRootFromJavaHome() {
		// search at the same directory as current JRE
		String javaHome = System.getProperty(JAVA_HOME_PROPERTY_KEY);
		for (File directory : getPossibleJdkRootDirectory(javaHome)) {
			String path = directory.getPath();
			if (null == validateJdkRootDirectory(path)) {
				//RemoteDebugActivator.pluginLog().logInfo(NLS.bind(Messages.jdkRootDirectoryFoundMsg, path));
				return path;
			}
		}
		return null;
	}

	/**
	 * Gets the directories that could be JDK root directory.
	 * 
	 * @param javaHome
	 *            The java home path
	 * @return The directories that could be JDK root directory.
	 */
	private static List<File> getPossibleJdkRootDirectory(String javaHome) {
		List<File> dirs = new ArrayList<File>();

		/*
		 * On Mac, java home path can be for example:
		 * /Library/Java/JavaVirtualMachines/jdk1.7.0_13.jdk/Contents/Home/jre
		 */

		if (Platform.getOS().equals(Platform.OS_MACOSX)) {
			int index = javaHome.indexOf(IToolsConstants.JAVA_INSTALLATION_DIR_ON_MAC);
			if (index == -1) {
				return dirs;
			}

			String javaVirtualMachinesPath = javaHome.substring(0,
					index + IToolsConstants.JAVA_INSTALLATION_DIR_ON_MAC.length());
			File dir = new File(javaVirtualMachinesPath);

			collectDirs(dirs, dir, 3);
			return dirs;
		}

		File parentDir = new File(javaHome + File.separator + ".."); //$NON-NLS-1$
		// JRE's most often live inside a jdk's jre folder
		dirs.add(parentDir);
		if (parentDir.exists()) {
			for (File file : parentDir.listFiles()) {
				if (file.isDirectory()) {
					dirs.add(file);
				}
			}
		}

		return dirs;
	}

	/**
	 * Collects the directories which are within given depth from given base
	 * directory.
	 * 
	 * @param dirs
	 *            The directories to store result
	 * @param dir
	 *            The directory to search
	 * @param depth
	 *            The depth to search
	 */
	private static void collectDirs(List<File> dirs, File dir, int depth) {
		if (depth > 0) {
			for (File file : dir.listFiles()) {
				if (file.isDirectory()) {
					dirs.add(file);
					collectDirs(dirs, file, depth - 1);
				}
			}
		}
	}

	/**
	 * Validates the class path and library path.
	 * 
	 * @return <tt>true</tt> if tools.jar (or classses.jar in Mac) can be found
	 *         in class path, and the required shared library can be also found.
	 */
	private boolean validateClassPathAndLibraryPath() {
		try {
			invokeGetMonitoredHost(LOCALHOST);
		} catch (ToolsCoreException e) {
			return false;
		}
		return true;
	}

	// Find the home directory based on the preference settings
	private IVMInstall findHomeDirFromPreferences() {
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(RemoteDebugActivator.PLUGIN_ID);
		String vm = prefs.get(IPropertyKeys.JDK_VM_INSTALL, null);
		if (vm != null) {
			IVMInstall found = findVMInstall(vm);
			if (found != null) {
				return found;
			}
		}
		return null;
	}

	private static IVMInstall findVMInstall(String id) {
		if (id != null) {
			IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			IVMInstall ret = null;
			for (int i = 0; i < types.length; i++) {
				ret = types[i].findVMInstall(id);
				if (ret != null)
					return ret;
			}
		}
		return null;
	}

	private static IVMInstall findVMByInstallLocation(String home) {
		if (home != null) {
			IVMInstallType[] types = JavaRuntime.getVMInstallTypes();
			for (int i = 0; i < types.length; i++) {
				IVMInstall[] installs = types[i].getVMInstalls();
				for (int j = 0; j < installs.length; j++) {
					if (home.equals(installs[j].getInstallLocation())) {
						return installs[j];
					}
				}
			}
		}
		return null;
	}

	private String findHomeDirectoryToAddToClasspath() {
		// Find the jdk root for the currently running java home
		String jdkRootDirectory = findJdkRootFromJavaHome();
		if (jdkRootDirectory == null || jdkRootDirectory.trim().isEmpty()) {
			IVMInstall vmi = findSecondaryVMInstall();
			if (vmi != null)
				jdkRootDirectory = vmi.getInstallLocation().getAbsolutePath();
		}
		return jdkRootDirectory;
	}

	/**
	 * Get the vminstall to use when the currently-running jre is not a jdk
	 * 
	 * @return
	 */
	public IVMInstall findSecondaryVMInstall() {
		IVMInstall vmi = findHomeDirFromPreferences();
		if (vmi == null) {
			vmi = findFirstValidVMInstall();
		}
		return vmi;
	}

	/**
	 * Find the IVMInstall who's home directory matches the current java.home
	 * sysprop
	 * 
	 * @return
	 */
	public IVMInstall findActiveVM() {
		String javaHome = System.getProperty(JAVA_HOME_PROPERTY_KEY);
		return findVMByInstallLocation(javaHome);
	}

	/**
	 * Does an IVMInstall currently exist for the currently-running java.home
	 * 
	 * @return
	 */
	public boolean hasActiveVMInstall() {
		return findActiveVM() != null;
	}

	/**
	 * Gets the JRE library path.
	 * 
	 * @param jdkRootDirectory
	 *            The JDK root directory
	 * @return The JRE library path or <tt>null</tt> it not found
	 */
	private static String getJreLibraryPath(String jdkRootDirectory) {
		for (String path : LIBRARY_PATHS) {
			File attachLibraryFile = new File(jdkRootDirectory + path + File.separator
					+ System.mapLibraryName(ATTACH_LIBRARY));
			if (attachLibraryFile.exists()) {
				return jdkRootDirectory + path;
			}
		}
		return null;
	}

	/*
	 * Below are the commands that require sun classes
	 */

	/**
	 * Invokes the getAgentProperties method of VirtualMachine
	 * 
	 * @param virtualMachine
	 *            The virtual machine
	 * @return The agent properties
	 * @throws ToolsCoreException
	 */
	public Properties invokeGetAgentProperties(Object virtualMachine) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(GET_AGENT_PROPERTIES_METHOD);
			return (Properties) method.invoke(virtualMachine);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the getMonitoredHost method of MonitoredHost with reflection.
	 * 
	 * @param name
	 *            The host name
	 * @return The monitored host
	 * @throws ToolsCoreException
	 */
	synchronized public Object invokeGetMonitoredHost(String hostname) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(MONITORED_HOST_CLASS);
			Method method = clazz.getDeclaredMethod(GET_MONITORED_HOST_CLASS, new Class[] { String.class });
			return method.invoke(null, hostname);
		} catch (Throwable t) {
			t.printStackTrace();
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	public Set<Integer> invokeActiveVms(String hostname) throws ToolsCoreException {
		return invokeActiveVms(invokeGetMonitoredHost(hostname));
	}

	/**
	 * Invokes the activeVms method of MonitoredHost with reflection.
	 * 
	 * @param monitoredHost
	 *            The monitored host
	 * @return The active VMs.
	 * @throws ToolsCoreException
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> invokeActiveVms(Object monitoredHost) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(MONITORED_HOST_CLASS);
			Method method = clazz.getDeclaredMethod(ACTIVE_VMS_METHOD);
			return (Set<Integer>) method.invoke(monitoredHost);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the constructor of VmIdentifier with reflection.
	 * 
	 * @param vmId
	 *            The VM id.
	 * @return the VM identifier
	 * @throws ToolsCoreException
	 */
	public Object invokeVmIdentifier(String vmId) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Constructor<?> clazz = getToolsLoader().loadClass(VM_IDENTIFIER_CLASS).getConstructor(
					new Class[] { String.class });
			return clazz.newInstance(vmId);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the getMonitoredVm of MonitoredHost with reflection.
	 * 
	 * @param monitoredHost
	 *            The monitored host
	 * @param vmIdentifier
	 *            The VM identifier
	 * @return The monitored VM
	 * @throws ToolsCoreException
	 */
	synchronized public Object invokeGetMonitoredVm(Object monitoredHost, Object vmIdentifier)
			throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(MONITORED_HOST_CLASS);
			Class<?> clazz2 = getToolsLoader().loadClass(VM_IDENTIFIER_CLASS);
			Method method = clazz.getDeclaredMethod(GET_MONITORED_VM_METHOD, new Class[] { clazz2 });
			return method.invoke(monitoredHost, vmIdentifier);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/*
	 * Convenience method
	 */
	public Object getMonitoredVm(String hostname, int pid) throws ToolsCoreException {
		Object monitoredVm = invokeGetMonitoredVm(invokeGetMonitoredHost(hostname),
				invokeVmIdentifier(String.format(IToolsConstants.VM_IDENTIFIRER, pid)));
		return monitoredVm;
	}

	/**
	 * Invokes the findByName method of MonitoredVm with reflection.
	 * 
	 * @param monitoredVm
	 *            The monitored VM
	 * @param name
	 *            The name
	 * @return The monitor
	 * @throws ToolsCoreException
	 */
	public Object invokeFindByName(Object monitoredVm, String name) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(MONITORED_VM_CLASS);
			Method method = clazz.getDeclaredMethod(FIND_BY_NAME_METHOD, new Class[] { String.class });
			return method.invoke(monitoredVm, name);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the getValue method of Monitor with reflection.
	 * 
	 * @param monitor
	 *            The monitor
	 * @return The value
	 * @throws ToolsCoreException
	 */
	public Object invokeGetValue(Object monitor) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(MONITOR_CLASS);
			Method method = clazz.getDeclaredMethod(GET_VALUE_METHOD);
			return method.invoke(monitor);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the attach method of VirtualMachine with reflection.
	 * 
	 * @param pid
	 *            The process ID
	 * @return The virtual machine
	 * @throws ToolsCoreException
	 */
	public Object invokeAttach(int pid) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(ATTACH_METHOD, new Class[] { String.class });
			return method.invoke(null, String.valueOf(pid));
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getCause().getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the detach method of VirtualMachine with reflection.
	 * 
	 * @param vm
	 *            The virtual machine
	 * @throws ToolsCoreException
	 */
	public void invokeDetach(Object vm) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(DETACH_METHOD);
			method.invoke(vm);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the getSystemProperties method of VirtualMachine with reflection.
	 * 
	 * @param vm
	 *            The virtual machine
	 * @return The system properties
	 * @throws ToolsCoreException
	 */
	public Object invokeGetSystemProperties(Object vm) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(GET_SYSTEM_PROPERTIES_METHOD);
			return method.invoke(vm);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the loadAgent method of VirtualMachine with reflection.
	 * 
	 * @param virtualMachine
	 *            The virtual machine
	 * @param path
	 *            The path for agent jar file
	 * @param options
	 *            The options given to agent
	 * @throws ToolsCoreException
	 */
	public void invokeLoadAgent(Object virtualMachine, String path, String options) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(LOAD_AGENT_METHOD, new Class[] { String.class, String.class });
			method.invoke(virtualMachine, path, options);
		} catch (Throwable t) {
			String message = t.getMessage();
			if (message == null) {
				Throwable cause = t.getCause();
				while (cause != null) {
					message = cause.getMessage();
					if (message != null) {
						break;
					}
					cause = cause.getCause();
				}
			}
			throw new ToolsCoreException(IStatus.ERROR, message, t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	/**
	 * Invokes the heapHisto method of HotSpotVirtualMachine with reflection.
	 * 
	 * @param virtualMachine
	 *            The virtual machine
	 * @param isLive
	 *            True to dump only live objects
	 * @return The input stream of heap histo
	 * @throws ToolsCoreException
	 */
	public InputStream invokeHeapHisto(Object virtualMachine, boolean isLive) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(HOT_SPOT_VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(HEAP_HISTO_METHOD, new Class[] { Object[].class });
			Object[] arg = new Object[] { isLive ? HEAP_HISTO_LIVE_OPTION : HEAP_HISTO_ALL_OPTION };
			return (InputStream) method.invoke(virtualMachine, (Object) arg);
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	public String getJvmArgs(String hostname, int vmPid) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Object vm = getMonitoredVm(hostname, vmPid);
			Class<?> vmUtil = toolsLoader.loadClass(MONITORED_VM_UTIL_CLASS); //$NON-NLS-1$
			Class<?> monitoredVm = toolsLoader.loadClass(MONITORED_VM_CLASS); //$NON-NLS-1$
			Method jvmArgs = vmUtil.getDeclaredMethod("jvmArgs", //$NON-NLS-1$
					new Class[] { monitoredVm });
			Object jvmArgsObj = jvmArgs.invoke(null, vm);
			return (String) jvmArgsObj;
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	public String getMainClass(String hostname, int vmPid) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Object vm = getMonitoredVm(hostname, vmPid);
			Class<?> vmUtil = toolsLoader.loadClass(MONITORED_VM_UTIL_CLASS); //$NON-NLS-1$
			Class<?> monitoredVm = toolsLoader.loadClass(MONITORED_VM_CLASS); //$NON-NLS-1$

			Method mainClass = vmUtil.getDeclaredMethod("mainClass", new Class[] { monitoredVm, //$NON-NLS-1$
					boolean.class });
			Object mainClassObj = mainClass.invoke(null, new Object[] { vm, true });
			return (String) mainClassObj;
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}

	public String getMainArgs(String hostname, int vmPid) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Object vm = getMonitoredVm(hostname, vmPid);
			Class<?> vmUtil = toolsLoader.loadClass(MONITORED_VM_UTIL_CLASS); //$NON-NLS-1$
			Class<?> monitoredVm = toolsLoader.loadClass(MONITORED_VM_CLASS); //$NON-NLS-1$

			Method mainArgs = vmUtil.getDeclaredMethod("mainArgs", //$NON-NLS-1$
					new Class[] { monitoredVm });
			Object mainArgsObj = mainArgs.invoke(null, vm);
			return (String) mainArgsObj;
		} catch (Throwable t) {
			throw new ToolsCoreException(IStatus.ERROR, t.getMessage(), t);
		} finally {
			Thread.currentThread().setContextClassLoader(currentLoader);
		}
	}
	
	public String getJavaCommand(String host, int pid) throws ToolsCoreException  {
		return getJavaCommand(getMonitoredVm(host, pid), pid);
	}

	public String getJavaCommand(Object monitoredVm, int pid) throws ToolsCoreException  {
		String javaCommand = null;
		try {
			Object monitor = invokeFindByName(monitoredVm, IToolsConstants.JAVA_COMMAND_KEY);
			if (monitor != null) {
				javaCommand = tools.invokeGetValue(monitor).toString();
				return javaCommand == null ? "" : javaCommand;
			}
		} catch (ToolsCoreException e) {
			String message = NLS.bind(Messages.getMainClassNameFailed, pid);
			throw new ToolsCoreException(IStatus.ERROR, message, e);
		}
		return ""; //$NON-NLS-1$
	}

}
