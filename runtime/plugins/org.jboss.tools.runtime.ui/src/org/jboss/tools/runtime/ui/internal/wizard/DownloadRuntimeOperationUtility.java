/*******************************************************************************
 * Copyright (c) 2014 Red Hat 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.runtime.core.extract.ExtractUtility;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.dialogs.SearchRuntimePathDialog;

/**
 * Mixed class of core+ui to initiate the download, unzipping, 
 * and runtime creation for a downloaded runtime. 
 */
public class DownloadRuntimeOperationUtility {
	private static final String SEPARATOR = "/"; //$NON-NLS-1$

	private File getNextUnusedFilename(File destination, String name) {
		int i = 1;
		File file = new File (destination, name);
		while (file.exists()) {
			file = new File(destination, name + "(" + i++ + ")"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return file;
	}
	
	
	/**
	 * 
	 * @param downloadDestinationPath   The path to put the downloaded zip
	 * @param urlString					The remote url
	 * @param deleteOnExit				Whether to delete on exit or not
	 * @return
	 */
	private File getDestinationFile(String downloadDestinationPath, String urlString, boolean deleteOnExit) throws CoreException {
		File ret = null;
		try {
			URL url = new URL(urlString);
			String name = url.getPath();
			int slashIdx = name.lastIndexOf('/');
			if (slashIdx >= 0)
				name = name.substring(slashIdx + 1);

			File destination = new File(downloadDestinationPath);
			destination.mkdirs();
			ret = new File (destination, name);
			if (deleteOnExit) {
				ret = getNextUnusedFilename(destination, name);
			}
			if( deleteOnExit )
				ret.deleteOnExit();
			return ret;
		} catch (IOException e) {
			cancel(ret);
			IStatus s = new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, e.getMessage(), e);
			throw new CoreException(s);
		}
	}
	
	private boolean cacheOutdated(File local, boolean deleteOnExit, long urlLastModified) {
		boolean download = true;
		long urlModified = 0;
		if (!deleteOnExit) {
			long cacheModified = local.lastModified();
			download = cacheModified <= 0 || cacheModified != urlModified;
		}
		return download;
	}
	
	
	private long getRemoteURLModified(String urlString, String user, String pass, IProgressMonitor monitor) throws CoreException, IOException {
		monitor.beginTask("Checking remote timestamp", 100);
		long l = new URLTransportUtility().getLastModified(new URL(urlString), user, pass, monitor);
		monitor.worked(100);
		monitor.done();
		return l;
	}
	
	private void validateInputs(String downloadDirectoryPath, String unzipDirectoryPath) throws CoreException {
		File downloadDirectory = new File(downloadDirectoryPath);
		downloadDirectory.mkdirs();
		if (!downloadDirectory.isDirectory()) {
			throw new CoreException(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "The '" + downloadDirectory + "' is not a directory.")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		
		File unzipDirectory = new File(unzipDirectoryPath);
		unzipDirectory.mkdirs();
		if (!unzipDirectory.isDirectory()) {
			throw new CoreException( new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "The '" + unzipDirectory + "' is not a directory.")); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * This is a convenience method for providing no credentials.
	 * 
	 * @param selectedDirectory
	 * @param destinationDirectory
	 * @param urlString
	 * @param deleteOnExit
	 * @param monitor
	 * @return
	 * @deprecated
	 */
	public IStatus downloadAndInstall(String selectedDirectory, String destinationDirectory, 
			String urlString, boolean deleteOnExit, IProgressMonitor monitor) {
		return downloadAndInstall(selectedDirectory, destinationDirectory, urlString, deleteOnExit, null, null, null, monitor);
	}
	
	public IStatus downloadAndInstall(String unzipDirectoryPath, String downloadDirectoryPath, 
			String urlString, boolean deleteOnExit, String user, String pass, TaskModel tm, IProgressMonitor monitor) {
		monitor.beginTask("Configuring runtime from url " + urlString, 500);
		try {
			validateInputs(downloadDirectoryPath, unzipDirectoryPath);
			File downloadedFile = downloadRemoteRuntime(unzipDirectoryPath, downloadDirectoryPath, urlString, deleteOnExit, user, pass, new SubProgressMonitor(monitor, 450));
			
			ExtractUtility extractUtil = new ExtractUtility(downloadedFile);
			unzip(extractUtil, downloadedFile, unzipDirectoryPath, new SubProgressMonitor(monitor, 30));
			String updatedRuntimeRoot = getUpdatedUnzipPath(extractUtil, unzipDirectoryPath, new SubProgressMonitor(monitor, 10));
			tm.putObject(DownloadRuntimesWizard.UNZIPPED_SERVER_HOME_DIRECTORY, updatedRuntimeRoot);
			createRuntimes(updatedRuntimeRoot, new SubProgressMonitor(monitor, 10));
		} catch(CoreException ce) {
			return ce.getStatus();
		} finally {
			monitor.done();
		}
		return Status.OK_STATUS;
	}
	
	public IStatus downloadAndUnzip(String unzipDirectoryPath, String downloadDirectoryPath, 
			String urlString, boolean deleteOnExit, String user, String pass, TaskModel tm, IProgressMonitor monitor) {
		monitor.beginTask("Configuring runtime from url " + urlString, 500);
		try {
			validateInputs(downloadDirectoryPath, unzipDirectoryPath);
			File downloadedFile = downloadRemoteRuntime(unzipDirectoryPath, downloadDirectoryPath, urlString, deleteOnExit, user, pass, new SubProgressMonitor(monitor, 450));
			ExtractUtility extractUtil = new ExtractUtility(downloadedFile);
			unzip(extractUtil, downloadedFile, unzipDirectoryPath, new SubProgressMonitor(monitor, 30));
			String updatedRuntimeRoot = getUpdatedUnzipPath(extractUtil, unzipDirectoryPath, new SubProgressMonitor(monitor, 10));
			tm.putObject(DownloadRuntimesWizard.UNZIPPED_SERVER_HOME_DIRECTORY, updatedRuntimeRoot);
		} catch(CoreException ce) {
			return ce.getStatus();
		} finally {
			monitor.done();
		}
		return Status.OK_STATUS;
	}

	
	private File downloadRemoteRuntime(String unzipDirectoryPath, String destinationDirectory, 
			String urlString, boolean deleteOnExit, String user, String pass, IProgressMonitor monitor) throws CoreException  {
		monitor.beginTask("Downloading " + urlString, 1000);
		File file = null;
		try {
			file = getDestinationFile(destinationDirectory, urlString, deleteOnExit);
			
			long urlModified = deleteOnExit ? 0 : getRemoteURLModified(urlString, user, pass, new SubProgressMonitor(monitor, 100));
			boolean download = cacheOutdated(file, deleteOnExit, urlModified);

			IStatus result = null;
			if (download) {
				result = downloadFileFromRemoteUrl(file, new URL(urlString), urlModified, user, pass, new SubProgressMonitor(monitor, 900));
			}
			if( !result.isOK())
				throw new CoreException(result);
			if (monitor.isCanceled())
				throw new CoreException(cancel(file));
			
			return file;
		} catch (IOException  e) {
			cancel(file);
			throw new CoreException(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, e.getMessage(), e));
		} finally {
			monitor.done();
		}
	}

	
	private void unzip(File downloadedFile, String unzipDirectoryPath, IProgressMonitor monitor) throws CoreException  {
		unzip(new ExtractUtility(downloadedFile), downloadedFile, unzipDirectoryPath, monitor);
	}
	
	private void unzip(ExtractUtility util, File downloadedFile, String unzipDirectoryPath, IProgressMonitor monitor) throws CoreException  {
		monitor.beginTask("Unzipping " + downloadedFile.getAbsolutePath(), 1000);
		if (monitor.isCanceled())
			throw new CoreException(cancel(downloadedFile));

		IOverwrite overwriteQuery = createOverwriteFileQuery();
		final IStatus status = util.extract(new File(unzipDirectoryPath), overwriteQuery, new SubProgressMonitor(monitor, 1000));

		if (monitor.isCanceled())
			throw new CoreException( cancel(downloadedFile));
		if( !status.isOK())
			throw new CoreException(status);
	}
	
	private IStatus cancel(File f) {
		if( f != null ) {
			f.deleteOnExit();
			f.delete();
		}
		return Status.CANCEL_STATUS;
	}
	
	private static IStatus createRuntimes(String directory, IProgressMonitor monitor) {
		monitor.subTask("Creating runtime from location " + directory); //$NON-NLS-1$
		final RuntimePath runtimePath = new RuntimePath(directory);
		List<RuntimeDefinition> runtimeDefinitions = RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(runtimePath);
		if (runtimeDefinitions.size() == 0) {
			return new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_No_runtime_server_found);
		} else if (runtimeDefinitions.size() > 1) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					SearchRuntimePathDialog.launchSearchRuntimePathDialog(
							Display.getDefault().getActiveShell(),
							RuntimeUIActivator.getDefault().getModel().getRuntimePaths(), false, 7);
				}
			});
		} else /* size == 1 */{
			RuntimeInitializerUtil.initializeRuntimes(runtimeDefinitions);
		}
		monitor.done();
		return Status.OK_STATUS;
	}

	private String getUpdatedUnzipPath(ExtractUtility util, String unzipDirectoryPath, IProgressMonitor monitor) throws CoreException {
		try {
			String root = util.getExtractedRootFolder( new SubProgressMonitor(monitor, 10));
			if (root != null) {
				File rootFile = new File(unzipDirectoryPath, root);
				if (rootFile != null && rootFile.exists()) {
					unzipDirectoryPath = rootFile.getAbsolutePath();
				}
			}
			return unzipDirectoryPath;
		} catch(IOException ce) {
			cancel(util.getOriginalFile());
			throw new CoreException(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, ce.getMessage(), ce));
		} finally {
			monitor.done();
		}
	}
	
	private static IOverwrite createOverwriteFileQuery() {
		IOverwrite overwriteQuery = new IOverwrite() {
			public int overwrite(File file) {
				final String msg = NLS.bind(Messages.DownloadRuntimesSecondPage_The_file_already_exists, file.getAbsolutePath()); 
				final String[] options = { IDialogConstants.YES_LABEL,
						IDialogConstants.YES_TO_ALL_LABEL,
						IDialogConstants.NO_LABEL,
						IDialogConstants.NO_TO_ALL_LABEL,
						IDialogConstants.CANCEL_LABEL };
				final int[] retVal = new int[1];
				Display.getDefault().syncExec(new Runnable() {
					public void run() {
						Shell shell = PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
						MessageDialog dialog = new MessageDialog(shell, Messages.DownloadRuntimesSecondPage_Question,
								null, msg, MessageDialog.QUESTION, options, 0) {
							protected int getShellStyle() {
								return super.getShellStyle() | SWT.SHEET;
							}
						};
						dialog.open();
						retVal[0] = dialog.getReturnCode();
					}
				});
				return retVal[0];
			}
		};
		return overwriteQuery;
	}
	

	private IStatus downloadFileFromRemoteUrl(File toFile, URL url, long remoteUrlModified, String user, String pass, IProgressMonitor monitor) throws IOException {
		OutputStream out = null;
		try {
			out = new BufferedOutputStream(new FileOutputStream(toFile));
			IStatus result = new URLTransportUtility().download(
					toFile.getName(), url.toExternalForm(), user, pass, out, -1, monitor);
			out.flush();
			out.close();
			if (remoteUrlModified > 0) {
				toFile.setLastModified(remoteUrlModified);
			}
			return result;
		} finally { 
			if (out != null) {
				try {
					out.close();
				} catch (IOException e) {
					// ignore
				}
			}			
		}
	}

	
}
