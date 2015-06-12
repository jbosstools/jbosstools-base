/*******************************************************************************
 * Copyright (c) 2015 Red Hat 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.runtime.core.model.internal;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeWorkflowConstants;
import org.jboss.tools.runtime.core.model.IRuntimeInstaller;
import org.jboss.tools.runtime.core.util.internal.DownloadRuntimeOperationUtility;

/**
 * A runtime installer that launches the java -jar command on the downloaded file
 * 
 */
public class JavaJarRuntimeInstaller implements IRuntimeInstaller {

	public static final String ID = IRuntimeInstaller.JAVA_JAR_INSTALLER;
	
	private static final String EXTERNAL_LAUNCH_CONFIG_TYPE = "org.eclipse.ui.externaltools.ProgramLaunchConfigurationType"; //$NON-NLS-1$
	private static final String EXTERNAL_LAUNCH_ATTR_LOCATION = "org.eclipse.ui.externaltools.ATTR_LOCATION"; //$NON-NLS-1$
	private static final String EXTERNAL_LAUNCH_ATTR_ARGS = "org.eclipse.ui.externaltools.ATTR_TOOL_ARGUMENTS"; //$NON-NLS-1$
	
	
	public JavaJarRuntimeInstaller() {
		// TODO Auto-generated constructor stub
	}
	

	@Override
	public IStatus installRuntime(DownloadRuntime downloadRuntime, String unzipDirectory, String downloadDirectory,
			boolean deleteOnExit, TaskModel taskModel, IProgressMonitor monitor) {
		
		String user = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.USERNAME_KEY);
		String pass = (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.PASSWORD_KEY);
		
		monitor.beginTask("Install Runtime '" + downloadRuntime.getName() + "' ...", 100);//$NON-NLS-1$ //$NON-NLS-2$
		monitor.worked(1);
		try {
			File f = new DownloadRuntimeOperationUtility().download(unzipDirectory, downloadDirectory, 
					getDownloadUrl(downloadRuntime, taskModel), deleteOnExit, user, pass, taskModel, new SubProgressMonitor(monitor, 80));
			ILaunchConfiguration lc = createExternalToolsLaunchConfiguration(f, unzipDirectory);
			ILaunch launch = lc.launch("run", new NullProgressMonitor());
			if( launch == null ) {
				return new Status(IStatus.ERROR, RuntimeCoreActivator.PLUGIN_ID, "Unable to launch external command java -jar " + f.getAbsolutePath());
			}
			IProcess[] processes = launch.getProcesses();
			boolean finished = false;
			while(!monitor.isCanceled() && !finished) {
				boolean checkFinished = true;
				for( int i = 0; i < processes.length; i++ ) {
					checkFinished &= processes[i].isTerminated();
				}
				finished = checkFinished;
				try { 
					Thread.sleep(500);
				} catch(InterruptedException ie) {
					// Ignore
				}
			}
		} catch(CoreException ce) {
			return ce.getStatus();
		}
		return Status.OK_STATUS;
	}

	private String getDownloadUrl(DownloadRuntime downloadRuntime, TaskModel taskModel) {
		if( downloadRuntime != null ) {
			String dlUrl = downloadRuntime.getUrl();
			if( dlUrl == null ) {
				return (String)taskModel.getObject(IDownloadRuntimeWorkflowConstants.DL_RUNTIME_URL);
			}
			return dlUrl;
		}
		return null;
	}
	
	
	static final String JAVA_HOME_PROPERTY_KEY = "java.home";
	private ILaunchConfiguration createExternalToolsLaunchConfiguration(File downloadedFile, String unzipDirectory)
			throws CoreException {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(EXTERNAL_LAUNCH_CONFIG_TYPE);
		ILaunchConfigurationWorkingCopy wc = type.newInstance(null, manager.generateLaunchConfigurationName("java -jar " + downloadedFile.getAbsolutePath()));
		
		IVMInstall install = JavaRuntime.getDefaultVMInstall();
		File javaHome = null;
		if( install != null ) {
			javaHome = install.getInstallLocation();
		} else {
			String jHome = System.getProperty(JAVA_HOME_PROPERTY_KEY);
			javaHome = new File(jHome);
		}
		IPath path = new Path(javaHome.getAbsolutePath());
		if( Platform.getOS().equals(Platform.OS_WIN32)) {
			path = path.append("bin").append("java.exe");
		} else {
			path = path.append("bin").append("java");
		}
		
		wc.setAttribute(EXTERNAL_LAUNCH_ATTR_LOCATION, path.toOSString());
		wc.setAttribute(EXTERNAL_LAUNCH_ATTR_ARGS, "-DINSTALL_PATH=\"" + unzipDirectory + "\"  -jar " + downloadedFile.getAbsolutePath());
		ILaunchConfiguration config = wc.doSave();
		return config;
	}

	
}
