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
package org.jboss.tools.common.jdt.debug.tools;

import java.io.File;
import java.io.InputStream;
import java.util.Properties;
import java.util.Set;

import org.eclipse.jdt.launching.IVMInstall;
import org.jboss.tools.common.jdt.debug.JavaUtilities;
import org.jboss.tools.common.jdt.debug.tools.internal.IToolsConstants;
import org.jboss.tools.common.jdt.debug.tools.internal.PreJigsawUtility;
import org.jboss.tools.common.jdt.debug.tools.internal.Tools;

/**
 * The API for accessing tools.jar classes
 * 
 */
public class ToolsCore {

	public static boolean isToolsReady() {
		return Tools.getInstance().isReady();
	}

	public static File getToolsJar() {
		return Tools.getInstance().getToolsJarFile();
	}
	
	public static String validateJdkRootDirectory(String jdkRootDirectory) {
		return new PreJigsawUtility().validateJdkRootDirectory(jdkRootDirectory);
	}

	public static IVMInstall[] getAllCompatibleInstalls() {
		return JavaUtilities.getAllCompatibleInstalls();
	}

	/**
	 * Find the vminstall from which to locate tools.jar and other libraries to
	 * use when the currently running vm is not a jdk.
	 * 
	 * @return
	 */
	public static IVMInstall getJdkVMInstall() {
		return new PreJigsawUtility().findSecondaryVMInstall();
	}

	
	/*
	 * Some simple wrapper classes so we're not passing Object all over
	 */
	

	public static class AttachedVM {
		private Object o;
		public AttachedVM(Object o) {
			this.o = o;
		}
		public Object getVM() {
			return o;
		}
	}
	
	public static class MonitoredHost {
		private Object o;
		public MonitoredHost(Object o) {
			this.o = o;
		}
		public Object getHost() {
			return o;
		}
	}
	
	public static class MonitoredVM {
		private Object o;
		public MonitoredVM(Object o) {
			this.o = o;
		}
		public Object getMonitoredVM() {
			return o;
		}
	}
	
	/*
	 * Entry-point APIs
	 */
	
	public static Set<Integer> getActiveVmPids(String hostname) throws ToolsCoreException {
		return Tools.getInstance().invokeActiveVms(hostname);
	}
	
	/**
	 * This method was poorly named and is is instead desired to return whether a process
	 * is able to be monitored or not. 
	 * 
	 * @param hostname
	 * @param vmPid
	 * @return
	 * @throws ToolsCoreException
	 */
	@Deprecated
	public static boolean processIsRunning(String hostname, int vmPid) throws ToolsCoreException {
		return processIsMonitorable(hostname, vmPid);
	}
	
	public static boolean processIsMonitorable(String hostname, int vmPid) throws ToolsCoreException {
		// If these throw exceptions, we want them to be rethrown and logged
		Object host = Tools.getInstance().invokeGetMonitoredHost(hostname);
		Object vmId = Tools.getInstance().invokeVmIdentifier(String.format(IToolsConstants.VM_IDENTIFIRER, vmPid));
		
		try {
			// We want to catch and ignore these failures and just return that the process is able to be monitored
			Object monitoredVm = Tools.getInstance().invokeGetMonitoredVm(host, vmId);
			return monitoredVm != null;
		} catch(ToolsCoreException tce) {
			return false;
		}
	}

	public static String getJvmArgs(String hostname, int vmPid) throws ToolsCoreException {
		return Tools.getInstance().getJvmArgs(hostname, vmPid);
	}

	public static String getMainClass(String hostname, int vmPid) throws ToolsCoreException {
		return Tools.getInstance().getMainClass(hostname, vmPid);
	}

	public static String getMainArgs(String hostname, int vmPid) throws ToolsCoreException {
		return Tools.getInstance().getMainArgs(hostname, vmPid);
	}
	
    public static String getJavaCommand(String hostname, int pid) throws ToolsCoreException  {
    	return Tools.getInstance().getJavaCommand(hostname, pid);
    }

	
	
	public static AttachedVM attach(int pid) throws ToolsCoreException {
		Object o = Tools.getInstance().invokeAttach(pid);
		return o == null ? null : new AttachedVM(o);
	}
	
	public static void detach(AttachedVM vm) throws ToolsCoreException {
		Tools.getInstance().invokeDetach(vm.getVM());
	}
	
    public static void loadAgent(AttachedVM virtualMachine, String path,
            String options) throws ToolsCoreException {
    	Tools.getInstance().invokeLoadAgent(virtualMachine.getVM(), path, options);
    }
    
    public static String startLocalManagementAgent(AttachedVM virtualMachine) throws ToolsCoreException {
    	return Tools.getInstance().invokeStartLocalManagementAgent(virtualMachine.getVM());
    }

    public static Properties getSystemProperties(AttachedVM vm) throws ToolsCoreException {
    	return (Properties) Tools.getInstance().invokeGetSystemProperties(vm.getVM());
    }

    public static Properties getAgentProperties(AttachedVM virtualMachine)
            throws ToolsCoreException {
    	return Tools.getInstance().invokeGetAgentProperties(virtualMachine.getVM());
    }
    
    public static MonitoredHost getMonitoredHost(String hostname)
            throws ToolsCoreException {
    	Object o = Tools.getInstance().invokeGetMonitoredHost(hostname);
    	return o == null ? null : new MonitoredHost(o);
    }
    
    public static MonitoredVM getMonitoredVm(String hostname, int pid) throws ToolsCoreException {
    	Object o = Tools.getInstance().getMonitoredVm(hostname, pid);
    	return o == null ? null : new MonitoredVM(o);

    }
    public static Set<Integer> getActiveProcessIds(String host)  throws ToolsCoreException  {
    	return Tools.getInstance().invokeActiveVms(host);
    }
    
	public static InputStream getHeapHistogram(AttachedVM virtualMachine, boolean isLive) throws ToolsCoreException {
		return Tools.getInstance().invokeHeapHisto(virtualMachine.getVM(), isLive);
	}
    
    public static boolean isJigsawRunning(AttachedVM virtualMachine) throws ToolsCoreException {
        return JavaUtilities.isJigsawRunning(ToolsCore.getSystemProperties(virtualMachine).getProperty("java.version"));
    }
}


