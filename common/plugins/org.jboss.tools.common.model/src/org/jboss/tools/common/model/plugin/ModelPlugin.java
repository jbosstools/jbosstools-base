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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchWindow;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.jboss.tools.common.model.XJob;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.osgi.framework.BundleContext;

public class ModelPlugin extends BaseUIPlugin implements IModelPlugin, IWindowListener {
	public static final String PLUGIN_ID = "org.jboss.tools.common.model";  //$NON-NLS-1$
	private static ModelPlugin plugin;
	private ResourceBundle resourceBundle;
	private XModelSaveParticipant save = new XModelSaveParticipant();
	public static final String TEMP_FILE_PREFIX = "efs_";
	File tempFolder;

	public ModelPlugin() {
		plugin = this;
		try {
			File f = File.createTempFile(TEMP_FILE_PREFIX, ".x"); //$NON-NLS-1$ //$NON-NLS-2$
			tempFolder = f.getParentFile();
			f.deleteOnExit();
		} catch (IOException e) {
			getPluginLog().logError("ModelPlugin:requestForTempFolder:" + e.getMessage()); //$NON-NLS-1$
		}
	}

	public static ModelPlugin getDefault() {
		return plugin;
	}

	public File getTempFolder() {		
		return tempFolder;
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
	}
	
	protected void initializeDefaultPluginPreferences() {
	}

	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		cleanTempFiles();
		XJob.shutdown();
	}
	
	private void cleanTempFiles() {
		if(tempFolder != null && tempFolder.exists()) {
			File[] fs = tempFolder.listFiles();
			if(fs != null) for (int i = 0; i < fs.length; i++) {
				String n = fs[i].getName();
				if(n.startsWith("efs_")) fs[i].delete(); //$NON-NLS-1$
			}
		}
	}

	public void windowActivated(IWorkbenchWindow window) {}

	public void windowClosed(IWorkbenchWindow window) {
		try {
			save.saving(null);
		} catch (CoreException e) {
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
	
	public static boolean isUTF8BOM(String encoding, IFile file) throws CoreException {
		if ("UTF-8".equals(encoding) && file != null && file.exists()) { //$NON-NLS-1$
			IContentDescription description= file.getContentDescription();
			if (description != null) {
				byte[] bom= (byte[]) description.getProperty(IContentDescription.BYTE_ORDER_MARK);
				if (bom != null) {
					if (bom != IContentDescription.BOM_UTF_8)
						throw new CoreException(new Status(IStatus.ERROR, ModelPlugin.PLUGIN_ID, IStatus.OK,"wrongByteOrderMark", null)); //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}
	
	public static String getContent(InputStream contentStream, String encoding, boolean skipUTF8BOM) throws CoreException {
		Reader in= null;
		
		try {
			if (skipUTF8BOM) {
				for (int i= 0; i < 3; i++)
					if (contentStream.read() == -1) {
						throw new IOException("notEnoughBytesForBOM"); //$NON-NLS-1$
				}
			}

			final int DEFAULT_FILE_SIZE= 15 * 1024;
			if (encoding == null)
				in= new BufferedReader(new InputStreamReader(contentStream), DEFAULT_FILE_SIZE);
			else
				in= new BufferedReader(new InputStreamReader(contentStream, encoding), DEFAULT_FILE_SIZE);
			StringBuffer buffer= new StringBuffer(DEFAULT_FILE_SIZE);
			char[] readBuffer= new char[2048];
			int n= in.read(readBuffer);
			while (n > 0) {
				buffer.append(readBuffer, 0, n);
				n= in.read(readBuffer);
			}

			return buffer.toString();

		} catch (IOException x) {
			throw new CoreException(new Status(IStatus.ERROR, ModelPlugin.PLUGIN_ID, IStatus.OK, "Failed to access or read underlying storage", x)); //$NON-NLS-1$
		} finally {
			try {
				if (in != null)
					in.close();
				else
					contentStream.close();
			} catch (IOException ignored) {
			}
		}
	}
}