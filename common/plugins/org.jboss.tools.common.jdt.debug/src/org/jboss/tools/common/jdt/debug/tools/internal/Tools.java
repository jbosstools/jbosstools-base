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
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.common.jdt.debug.IPropertyKeys;
import org.jboss.tools.common.jdt.debug.JavaUtilities;
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

			// tools.jar classes are available directly on classpath in java9 and above.
			if( !JavaUtilities.isJigsawRunning()) {
				PreJigsawUtility util = new PreJigsawUtility();
				File toolsJar = util.getToolsJarFile();
				if( toolsJar != null ) {
					toolsLoader = util.getToolsJarClassloader(toolsJar, toolsLoader);
				}
			}
		}
		return toolsLoader;
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
					//FIXME: DIRTY Workaround
					if (isAgentLoadExceptionWithReturnCode0(cause)) {
						return;
					}
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
	 * Invokes the startLocalManagementAgent method of VirtualMachine with reflection.
	 * 
	 * @param virtualMachine
	 *            The virtual machine
	 * @throws ToolsCoreException
	 */
	public String invokeStartLocalManagementAgent(Object virtualMachine) throws ToolsCoreException {
		ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(getToolsLoader());
			Class<?> clazz = getToolsLoader().loadClass(VIRTUAL_MACHINE_CLASS);
			Method method = clazz.getDeclaredMethod(START_LOCAL_MANAGEMENT_AGE_METHOD, new Class[] {});
			return (String) method.invoke(virtualMachine);
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

	private boolean isAgentLoadExceptionWithReturnCode0(Throwable cause) {
		return cause.getClass().getName().contains("AgentLoadException") && "0".equals(cause.getMessage());
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
