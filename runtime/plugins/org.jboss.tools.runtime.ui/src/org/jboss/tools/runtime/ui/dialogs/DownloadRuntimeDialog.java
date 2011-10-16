/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.dialogs;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.project.examples.ProjectExamplesActivator;
import org.jboss.tools.project.examples.filetransfer.ECFExamplesTransport;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * @author snjeza
 * 
 */
public class DownloadRuntimeDialog extends Dialog {

	private static final String DELETE_ON_EXIT = "deleteOnExit";
	private static final String JAVA_IO_TMPDIR = "java.io.tmpdir";
	private static final String USER_HOME = "user.home";
	private static final String DEFAULT_DIALOG_PATH = "defaultDialogPath";
	private static final String DEFAULT_DESTINATION_PATH = "defaultDestinationPath";
	private IDialogSettings dialogSettings;
	private Button deleteOnExit;
	private Text destinationPathText;
	private Text pathText;
	private DownloadRuntime downloadRuntime;
	private String delete;
	
	public DownloadRuntimeDialog(Shell parentShell, DownloadRuntime downloadRuntime) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		this.downloadRuntime = downloadRuntime;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		getShell().setText("Download Runtime '" + downloadRuntime.getName() + "'");
		Composite area = (Composite) super.createDialogArea(parent);
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		//gd.heightHint = 200;
		gd.widthHint = 600;
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		applyDialogFont(contents);
		initializeDialogUnits(area);

		Composite pathComposite = new Composite(contents, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathComposite.setLayoutData(gd);
		pathComposite.setLayout(new GridLayout(3, false));
		
		
		Label pathLabel = new Label(pathComposite, SWT.NONE);
		pathLabel.setText("Installation directory:");
		
		pathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathText.setLayoutData(gd);
		String defaultPath = dialogSettings.get(DEFAULT_DIALOG_PATH);
		if (defaultPath == null || defaultPath.isEmpty()) {
			defaultPath=System.getProperty(USER_HOME);
		}
		pathText.setText(defaultPath);
		pathText.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});
		
		Button browseButton = new Button(pathComposite, SWT.NONE);
		browseButton.setText("Browse...");
		browseButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(getShell());
				dialog.setMessage("Select installation directory");
				dialog.setFilterPath(pathText.getText());
				final String path = dialog.open();
				if (path == null) {
					return;
				}
				pathText.setText(path);
			}
		
		});
		
		Label destinationLabel = new Label(pathComposite, SWT.NONE);
		destinationLabel.setText("Destination directory:");
		
		destinationPathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		destinationPathText.setLayoutData(gd);
		String destinationPath = dialogSettings.get(DEFAULT_DESTINATION_PATH);
		if (destinationPath == null || destinationPath.isEmpty()) {
			destinationPath=System.getProperty(JAVA_IO_TMPDIR);
		}
		destinationPathText.setText(destinationPath);
		
		Button browseDestinationButton = new Button(pathComposite, SWT.NONE);
		browseDestinationButton.setText("Browse...");
		browseDestinationButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(getShell());
				dialog.setMessage("Select destination directory");
				dialog.setFilterPath(destinationPathText.getText());
				final String path = dialog.open();
				if (path == null) {
					return;
				}
				destinationPathText.setText(path);
			}
		
		});
		
		destinationPathText.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});
		
		deleteOnExit = new Button(pathComposite, SWT.CHECK);
		deleteOnExit.setText("Delete archive after installing");
		
		delete = dialogSettings.get(DELETE_ON_EXIT);
		if (delete == null) {
			delete = "true";
		}
		deleteOnExit.setSelection(new Boolean(delete));
		deleteOnExit.addSelectionListener(new SelectionAdapter() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				delete = new Boolean(deleteOnExit.getSelection()).toString();
			}
			
		});
		
		return area;
	}

	protected void validate() {
		getButton(IDialogConstants.OK_ID).setEnabled(true);
		if (pathText.getText().isEmpty()) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}
		if (destinationPathText.getText().isEmpty()) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}
	}

	@Override
	protected void okPressed() {
		dialogSettings.put(DEFAULT_DESTINATION_PATH,
				destinationPathText.getText());
		dialogSettings.put(DEFAULT_DIALOG_PATH, pathText.getText());
		dialogSettings.put(DELETE_ON_EXIT, delete);
		String selectedDirectory = pathText.getText();
		String destinationDirectory = destinationPathText.getText();
		boolean del = deleteOnExit.getSelection();
		super.okPressed();
		downloadRuntime(selectedDirectory, destinationDirectory, del);
	}

	private void downloadRuntime(final String selectedDirectory,
			final String destinationDirectory, final boolean deleteOnExit) {
		final ProgressMonitorDialog dialog = new ProgressMonitorDialog(getShell());
		dialog.setBlockOnOpen(false);
		dialog.setCancelable(true);
		dialog.open();
		final IProgressMonitor monitor = dialog.getProgressMonitor();
		monitor.beginTask("Download '" + downloadRuntime.getName() + "' ...", 100);
		
		IRunnableWithProgress runnable = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) {
				downloadAndInstall(selectedDirectory,
						destinationDirectory, deleteOnExit, monitor);
			}
		};
		try {
			dialog.run(true, true, runnable);
		} catch (Exception e) {
			RuntimeCoreActivator.log(e);
			MessageDialog.openError(getActiveShell(), "Error", e.getMessage());
		}
	}
	
	private IStatus downloadAndInstall(String selectedDirectory, String destinationDirectory, boolean deleteOnExit, IProgressMonitor monitor) {
		FileInputStream in = null;
		OutputStream out = null;
		File file = null;
		try {
			URL url = new URL(downloadRuntime.getUrl());
			String name = url.getPath();
			int slashIdx = name.lastIndexOf('/');
			if (slashIdx >= 0)
				name = name.substring(slashIdx + 1);
			
			File destination = new File(destinationDirectory);
			destination.mkdirs();
			file = new File (destination, name);
			int i = 1;
			while (file.exists()) {
				file = new File(destination, name + "(" + i++ + ")");
			}
			
			if (deleteOnExit) {
				file.deleteOnExit();
			}
			out = new BufferedOutputStream(
					new FileOutputStream(file));
			
			IStatus result = ECFExamplesTransport.getInstance().download(file.getName(),
					url.toExternalForm(), out, monitor);
			out.flush();
			out.close();
			if (monitor.isCanceled()) {
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			File directory = new File(selectedDirectory);
			directory.mkdirs();
			if (!directory.isDirectory()) {
				RuntimeCoreActivator.getDefault().getLog().log(result);
				MessageDialog.openError(getActiveShell(), "Error", "The '" + directory + "' is not a directory.");
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			ProjectExamplesActivator.extractZipFile(file, directory, monitor);
			if (!result.isOK()) {
				RuntimeCoreActivator.getDefault().getLog().log(result);
				String message;
				if (result.getException() != null) {
					message = result.getException().getMessage();
				} else {
					message = result.getMessage();
				}
				MessageDialog.openError(getActiveShell(), "Error", message);
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			createRuntimes(selectedDirectory, monitor);
		} catch (IOException e) {
			RuntimeCoreActivator.log(e);
			if (file != null && file.exists()) {
				file.deleteOnExit();
				file.delete();
			}
			MessageDialog.openError(getActiveShell(), "Error", e.getMessage());
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
					// ignore
				}
			}
			if (out != null) {
				try {
					out.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		return Status.OK_STATUS;
	}
	
	private Shell getActiveShell() {
		Display display = Display.getDefault();
		if (display != null) {
			return display.getActiveShell();
		}
		return null;
	}

	private static void createRuntimes(String directory, IProgressMonitor monitor) {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		Set<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault()
				.getRuntimePaths();
		RuntimePath newPath = new RuntimePath(directory);
		runtimePaths.add(newPath);
		for (RuntimePath runtimePath : runtimePaths) {
			List<ServerDefinition> serverDefinitions = locator
					.searchForRuntimes(runtimePath.getPath(),
							monitor);
			runtimePath.getServerDefinitions().clear();
			for (ServerDefinition serverDefinition : serverDefinitions) {
				serverDefinition.setRuntimePath(runtimePath);
			}
			runtimePath.getServerDefinitions().addAll(serverDefinitions);
			RuntimeUIActivator.getDefault().getRuntimePaths().add(runtimePath);
			RuntimeUIActivator.getDefault().saveRuntimePaths();
		}
		List<ServerDefinition> serverDefinitions = RuntimeUIActivator
				.getDefault().getServerDefinitions();
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator
				.getRuntimeDetectors();
		for (IRuntimeDetector detector : detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(serverDefinitions);
			}
		}
	}

}
