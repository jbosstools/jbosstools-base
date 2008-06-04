/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.plugin;

import java.io.File;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.osgi.framework.BundleContext;

public class ModelPlugin extends BaseUIPlugin implements IModelPlugin, IWindowListener {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model"; 
	private static ModelPlugin plugin;
	private ResourceBundle resourceBundle;
	private XModelSaveParticipant save = new XModelSaveParticipant();

	public ModelPlugin() {
		plugin = this;
	}

	public static ModelPlugin getDefault() {
		return plugin;
	}
	
	public static boolean isDebugEnabled() {
		return getDefault().isDebugging();
	}

	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	public static String getResourceString(String key) {
		ResourceBundle bundle = ModelPlugin.getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	
	public XModelSaveParticipant getSaveParticipant() {
		return save;
	}

	public void start(BundleContext context) throws Exception {
		System.setProperty(XModelConstants.HOME, EclipseResourceUtil.getInstallPath(context.getBundle()));
		super.start(context);		
		try {
			 /*
			    FIXME 	That's not right solution to obtain workbench because there could be no  workbench yet
			 			If so it leads to the exception below and problem with model plug-in activation 
			 	java.lang.IllegalStateException: Workbench has not been created yet.
		     	[java] 	at org.eclipse.ui.PlatformUI.getWorkbench(PlatformUI.java:92)
		     	[java] 	at org.jboss.tools.common.model.plugin.ModelPlugin.start(ModelPlugin.java:76)
		     	[java] 	at org.eclipse.osgi.framework.internal.core.BundleContextImpl$2.run(BundleContextImpl.java:1009)
		     	[java] 	at java.security.AccessController.doPrivileged(Native Method)
		     */

			PlatformUI.getWorkbench().addWindowListener(this);
		} catch (Exception e) {
			getPluginLog().logError(e);
		}
//		ClassLoaderUtil.init();
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				ClassLoaderUtil.init();
			}
		});
	}
	
	protected void initializeDefaultPluginPreferences() {
	}

	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		cleanTempFiles();
	}
	
	private void cleanTempFiles() {
		try {
			File f = File.createTempFile("efs_", ".x");
			f = f.getParentFile();
			File[] fs = f.listFiles();
			if(fs != null) for (int i = 0; i < fs.length; i++) {
				String n = fs[i].getName();
				if(n.startsWith("efs_")) fs[i].delete();
			}
		} catch (Exception e) {
			getPluginLog().logError("ModelPlugin:cleanTempFiles:" + e.getMessage());
		}
	}

	public void windowActivated(IWorkbenchWindow window) {}

	public void windowClosed(IWorkbenchWindow window) {
		try {
			save.saving(null);
		} catch (Exception e) {
			getPluginLog().logError(e);
		}
	}

	public void windowDeactivated(IWorkbenchWindow window) {
	}

	public void windowOpened(IWorkbenchWindow window) {
	}

	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}
}
