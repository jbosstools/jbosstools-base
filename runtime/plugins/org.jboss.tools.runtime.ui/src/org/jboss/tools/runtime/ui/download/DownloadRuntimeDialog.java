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
package org.jboss.tools.runtime.ui.download;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.fieldassist.ControlDecoration;
import org.eclipse.jface.fieldassist.FieldDecoration;
import org.eclipse.jface.fieldassist.FieldDecorationRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.progress.IProgressService;
import org.jboss.tools.common.zip.UnzipOperation;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.ECFTransport;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * @author snjeza
 * 
 */
public class DownloadRuntimeDialog extends Dialog {

	private static final String SELECTED_RUNTIME_REQUIRED = "A runtime must be selected";//$NON-NLS-1$
	private static final String SEPARATOR = "/"; //$NON-NLS-1$
	private static final String FOLDER_IS_REQUIRED = "This folder is required";
	private static final String FOLDER_IS_NOT_WRITABLE = "This folder does not exist or is not writable";
	private static final String DELETE_ON_EXIT = "deleteOnExit"; //$NON-NLS-1$
	private static final String JAVA_IO_TMPDIR = "java.io.tmpdir"; //$NON-NLS-1$
	private static final String USER_HOME = "user.home"; //$NON-NLS-1$
	private static final String DEFAULT_DIALOG_PATH = "defaultDialogPath"; //$NON-NLS-1$
	private static final String DEFAULT_DESTINATION_PATH = "defaultDestinationPath"; //$NON-NLS-1$
	private IDialogSettings dialogSettings;
	private Button deleteOnExit;
	private Text destinationPathText;
	private Text pathText;
	private DownloadRuntime downloadRuntime; 
	private List<DownloadRuntime> downloadRuntimes; 
	private String delete;
	private ControlDecoration decPathError;
	private ControlDecoration decPathReq;
	private ControlDecoration destinationPathError;
	private ControlDecoration destinationPathReq;
	private ControlDecoration selectRuntimeError;
	private Combo runtimesCombo;
	private Link urlText;
	private Group warningComposite;
	private Label warningLabel;
	private Link warningLink;
	
	public DownloadRuntimeDialog(Shell parentShell, DownloadRuntime downloadRuntime) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		this.downloadRuntime = downloadRuntime;
	}

	public DownloadRuntimeDialog(Shell parentShell, List<DownloadRuntime> downloadRuntimes) {
		super(parentShell);
		setShellStyle(SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER
				| SWT.RESIZE | getDefaultOrientation());
		dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		this.downloadRuntimes = downloadRuntimes;
		if (downloadRuntimes != null && downloadRuntimes.size() == 1) {
			downloadRuntime = downloadRuntimes.get(0);
		}
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		if (downloadRuntime != null) {
			getShell().setText("Download Runtime '" + downloadRuntime.getName() + "'");//$NON-NLS-1$ //$NON-NLS-2$
		} else {
			getShell().setText("Download Runtime");//$NON-NLS-1$
		}
		Composite area = (Composite) super.createDialogArea(parent);
		Composite contents = new Composite(area, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		//gd.widthHint = 690;
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		applyDialogFont(contents);
		initializeDialogUnits(area);

		
		Composite pathComposite = new Composite(contents, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathComposite.setLayoutData(gd);
		pathComposite.setLayout(new GridLayout(3, false));
		
		if (downloadRuntimes != null) {
			addRuntimeSelectionCombo(pathComposite);
		}
		
		Label urlLabel = new Label(pathComposite, SWT.NONE);
		urlLabel.setText("URL:");
		urlText = new Link(pathComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd.horizontalSpan=2;
		urlText.setLayoutData(gd);
		urlText.addSelectionListener( new SelectionAdapter( ) {

			public void widgetSelected( SelectionEvent e )
			{
				String t = e.text;
				String humanUrl = downloadRuntime.getHumanUrl();
				if (humanUrl != null && t.contains(humanUrl)) {
					IWorkbenchBrowserSupport support = PlatformUI.getWorkbench()
							.getBrowserSupport();
					try {
						URL url = new URL(humanUrl); //$NON-NLS-1$
						support.getExternalBrowser().openURL(url);
					} catch (Exception e1) {
						RuntimeUIActivator.log(e1);
					}
				}
				
			}
		} );

		Label pathLabel = new Label(pathComposite, SWT.NONE);
		pathLabel.setText("Install folder:");
		
		pathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathText.setLayoutData(gd);
		final String defaultPath = getDefaultPath();
		pathText.setText(defaultPath);
		decPathError = addDecoration(pathText, FieldDecorationRegistry.DEC_ERROR, FOLDER_IS_NOT_WRITABLE);
		decPathReq = addDecoration(pathText, FieldDecorationRegistry.DEC_REQUIRED, FOLDER_IS_REQUIRED);
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
				dialog.setMessage("Select install folder");
				dialog.setFilterPath(pathText.getText());
				final String path = dialog.open();
				if (path == null) {
					return;
				}
				pathText.setText(path);
			}
		
		});
		
		Label destinationLabel = new Label(pathComposite, SWT.NONE);
		destinationLabel.setText("Download folder:");
		
		destinationPathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		destinationPathText.setLayoutData(gd);
		String destinationPath = dialogSettings.get(DEFAULT_DESTINATION_PATH);
		destinationPathError = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_ERROR, FOLDER_IS_NOT_WRITABLE);
		destinationPathReq = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_REQUIRED, FOLDER_IS_REQUIRED);
		
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
				dialog.setMessage("Select download folder");
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
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd.horizontalSpan=3;
		deleteOnExit.setLayoutData(gd);
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

		warningComposite = new Group(pathComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd.horizontalSpan = 3;
		warningComposite.setLayoutData(gd);
		warningComposite.setLayout(new GridLayout(1, false));
		warningComposite.setText("Warning");
		
		warningLabel = new Label(warningComposite, SWT.NONE);
		warningLink = new Link(warningComposite, SWT.NONE);

		warningLink.addSelectionListener( new SelectionAdapter( ) {

			public void widgetSelected( SelectionEvent e )
			{
				String text = e.text;
				String humanUrl = downloadRuntime == null ? null : downloadRuntime.getHumanUrl();
				String linkUrl = null; 
				if (humanUrl != null && "link".equals(text)) {//$NON-NLS-1$
					linkUrl = humanUrl;
				} else if ("Show Details".equals(text)) {//$NON-NLS-1$
					linkUrl = "http://www.redhat.com/jboss/";//$NON-NLS-1$
				}
				
				if (linkUrl != null) {
					IWorkbenchBrowserSupport support = PlatformUI.getWorkbench()
							.getBrowserSupport();
					try {
						URL url = new URL(linkUrl); 
						support.getExternalBrowser().openURL(url);
					} catch (Exception e1) {
						RuntimeUIActivator.log(e1);
					}
				}
				
			}
		} );


		refresh();

		return area;
	}

	private void addRuntimeSelectionCombo(Composite parent) {
		if (downloadRuntimes == null || downloadRuntimes.size() < 2) {
			return;
		}
		
		Label selectRuntimeLabel = new Label(parent, SWT.NONE);
		selectRuntimeLabel.setText("Select a runtime to download");

		runtimesCombo = new Combo(parent, SWT.FLAT | SWT.BORDER | SWT.READ_ONLY);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd.horizontalSpan=2;
		runtimesCombo.setLayoutData(gd);
		for (DownloadRuntime r : downloadRuntimes) {
			runtimesCombo.add(r.getName());
		}
		
		runtimesCombo.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				int i = runtimesCombo.getSelectionIndex();
				downloadRuntime = downloadRuntimes.get(i);
				refresh();
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		selectRuntimeError = addDecoration(runtimesCombo, FieldDecorationRegistry.DEC_REQUIRED, SELECTED_RUNTIME_REQUIRED);
	}

	private void refresh() {
		if (downloadRuntime != null) {
			boolean requireManualDownload = false;
			if (downloadRuntime.getUrl() != null) {
				urlText.setText(downloadRuntime.getUrl());
			} else if (downloadRuntime.getHumanUrl() != null){
				urlText.setText("<a>"+downloadRuntime.getHumanUrl().trim()+"</a>");//$NON-NLS-1$ //$NON-NLS-2$
				requireManualDownload = true;
			} else {
				urlText.setText("");
			}
			boolean isDisclaimer = downloadRuntime.isDisclaimer();
			boolean showWarning = isDisclaimer || requireManualDownload;
			
			if (showWarning) {
				if (isDisclaimer) {
					warningLabel.setText("This is a community project and, as such is not supported with an SLA.");//$NON-NLS-1$
					warningLink.setText("If you're looking for fully supported, certified, enterprise middleware try JBoss Enterprise Middleware products. <a>Show Details</a>");//$NON-NLS-1$
				} else if (requireManualDownload) {
					warningLabel.setText("This runtime is only available as manual download.");//$NON-NLS-1$
					warningLink.setText("Please click on the download <a>link</a>.");//$NON-NLS-1$
				}
			}
			warningComposite.setVisible(showWarning);
		} else {
			warningComposite.setVisible(false);
		}
		warningComposite.getParent().getParent().layout(true, true);
		validate();
	}

	private String getDefaultPath() {
		String defaultPath = dialogSettings.get(DEFAULT_DIALOG_PATH);
		if (defaultPath == null || defaultPath.isEmpty()) {
			defaultPath=System.getProperty(USER_HOME);
		}
		return defaultPath;
	}

	private void showDecorations() {
		String path = pathText.getText();
		String destination = destinationPathText.getText();
		decPathError.hide();
		decPathReq.hide();
		destinationPathError.hide();
		destinationPathReq.hide();
		
		boolean hasRuntime = downloadRuntime != null;
		if (selectRuntimeError != null) {
			if (hasRuntime) {
				selectRuntimeError.hide();
			} else {
				selectRuntimeError.show();
			}
		}
		if (path.isEmpty()) {
			decPathReq.show();
		}
		if (destination.isEmpty()) {
			destinationPathReq.show();
		}
		boolean pathExists = checkPath(path, decPathError);
		boolean destExists = checkPath(destination, destinationPathError);
		getButton(IDialogConstants.OK_ID).setEnabled(pathExists
			&& destExists
			&& !path.isEmpty() && !destination.isEmpty()
			&& hasRuntime
			);
		decPathError.setShowHover(true);
	}

	private boolean checkPath(String path, ControlDecoration dec) {
		if (path.isEmpty()) {
			return true;
		}
		try {
			File file = File.createTempFile("temp", "txt", new File(path));//$NON-NLS-1$ //$NON-NLS-2$
			file.deleteOnExit();
			file.delete();
		} catch (IOException e) {
			dec.show();
			return false;
		}
		return true;
	}

	protected ControlDecoration addDecoration(Control control, String id, String description) {
		final ControlDecoration decPath = new ControlDecoration(control, SWT.TOP
				| SWT.LEFT);
		FieldDecorationRegistry registry = FieldDecorationRegistry.getDefault();
		FieldDecoration fd = registry.getFieldDecoration(id);
		decPath.setImage(fd.getImage());
		fd.setDescription(description);
	
		decPath.setImage(FieldDecorationRegistry.getDefault().getFieldDecoration(
				id).getImage());

		decPath.setShowOnlyOnFocus(false);
		decPath.setShowHover(true);
		decPath.setDescriptionText(description);
		return decPath;
	}

	protected void validate() {
		if (getContents() == null) {
			return;
		}
		getButton(IDialogConstants.OK_ID).setEnabled(true);
		if (pathText.getText().isEmpty()) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}
		if (destinationPathText.getText().isEmpty()) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}
		showDecorations();
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
		
		if (downloadRuntime.getUrl() == null || "".equals(downloadRuntime.getUrl().trim())) {//$NON-NLS-1$
			//nothing to do
			return;
		}
		
		Job downloadJob = new Job("Download '" + downloadRuntime.getName()) {//$NON-NLS-1$

			@Override
			public IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("Download '" + downloadRuntime.getName() + "' ...", 100);//$NON-NLS-1$ //$NON-NLS-2$
				downloadAndInstall(selectedDirectory, destinationDirectory, 
						downloadRuntime.getUrl(), deleteOnExit, monitor);
				return Status.OK_STATUS;
			}
			
		};
		downloadJob.setUser(false);
		downloadJob.schedule();
		IProgressService progressService= PlatformUI.getWorkbench().getProgressService();
		progressService.showInDialog(PlatformUI.getWorkbench().getModalDialogShellProvider().getShell(), downloadJob);
	}
	
	private IStatus downloadAndInstall(String selectedDirectory, String destinationDirectory, 
			String urlString, boolean deleteOnExit, IProgressMonitor monitor) {
		FileInputStream in = null;
		OutputStream out = null;
		File file = null;
		try {
			URL url = new URL(urlString);
			String name = url.getPath();
			int slashIdx = name.lastIndexOf('/');
			if (slashIdx >= 0)
				name = name.substring(slashIdx + 1);
			
			File destination = new File(destinationDirectory);
			destination.mkdirs();
			file = new File (destination, name);
			int i = 1;
			boolean download = true;
			long urlModified = 0;
			if (deleteOnExit) {
				while (file.exists()) {
					file = new File(destination, name + "(" + i++ + ")"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			} else {
				long cacheModified = file.lastModified();
				try {
					urlModified = ECFTransport.getInstance()
							.getLastModified(url);
					download = cacheModified <= 0 || cacheModified != urlModified;
				} catch (CoreException e) {
					// ignore
				}
			}
			if (deleteOnExit) {
				file.deleteOnExit();
			}
			IStatus result = null;
			if (download) {
				out = new BufferedOutputStream(new FileOutputStream(file));
				result = ECFTransport.getInstance().download(
						file.getName(), url.toExternalForm(), out, monitor);
				out.flush();
				out.close();
				if (urlModified > 0) {
					file.setLastModified(urlModified);
				}
			}
			if (monitor.isCanceled()) {
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			File directory = new File(selectedDirectory);
			directory.mkdirs();
			if (!directory.isDirectory()) {
				final String message = "The '" + directory + "' is not a directory.";//$NON-NLS-1$ //$NON-NLS-2$
				if (result != null) {
					RuntimeUIActivator.getDefault().getLog().log(result);
				} else {
					RuntimeUIActivator.getDefault().getLog().log(result);
				}
				Display.getDefault().syncExec(new Runnable() {

					@Override
					public void run() {
						MessageDialog.openError(getActiveShell(), "Error", message);//$NON-NLS-1$
					}
					
				});
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			new UnzipOperation(file).execute(directory);
			if (result != null && !result.isOK()) {
				RuntimeUIActivator.getDefault().getLog().log(result);
				final String message = getMessage(result);
				Display.getDefault().syncExec(new Runnable() {

					@Override
					public void run() {
						MessageDialog.openError(getActiveShell(), "Error", message);//$NON-NLS-1$
					}
					
				});
				file.deleteOnExit();
				file.delete();
				return Status.CANCEL_STATUS;
			}
			String root = getRoot(file, monitor);
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			if (root != null) {
				File rootFile = new File(selectedDirectory, root);
				if (rootFile != null && rootFile.exists()) {
					selectedDirectory = rootFile.getAbsolutePath();
				}
			}
			createRuntimes(selectedDirectory, monitor);
		} catch (IOException e) {
			RuntimeUIActivator.log(e);
			if (file != null && file.exists()) {
				file.deleteOnExit();
				file.delete();
			}
			final String message = e.getMessage();
			Display.getDefault().syncExec(new Runnable() {

				@Override
				public void run() {
					MessageDialog.openError(getActiveShell(), "Error", message);//$NON-NLS-1$
				}
				
			});
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

	private String getMessage(IStatus result) {
		String message;
		if (result.getException() != null) {
			message = result.getException().getMessage();
		} else {
			message = result.getMessage();
		}
		return message;
	}
	
	private String getRoot(File file, IProgressMonitor monitor) {
		ZipFile zipFile = null;
		String root = null;
		try {
			zipFile = new ZipFile(file);
			Enumeration<? extends ZipEntry> entries = zipFile.entries();
			while (entries.hasMoreElements()) {
				if (monitor.isCanceled()) {
					return null;
				}
				ZipEntry entry = (ZipEntry) entries.nextElement();
				String entryName = entry.getName();
				if (entryName == null || entryName.isEmpty() 
						|| entryName.startsWith(SEPARATOR) || entryName.indexOf(SEPARATOR) == -1) {
					return null;
				}
				String directory = entryName.substring(0, entryName.indexOf(SEPARATOR));
				if (root == null) {
					root = directory;
					continue;
				}
				if (!directory.equals(root)) {
					return null;
				}
			}
		} catch (IOException e) {
			RuntimeUIActivator.log(e);
			return null;
		} finally {
			if (zipFile != null) {
				try {
					zipFile.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		return root;
	}

	private Shell getActiveShell() {
		Display display = Display.getDefault();
		if (display != null) {
			return PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
		}
		return null;
	}

	private static void createRuntimes(String directory,
			IProgressMonitor monitor) {
		final RuntimePath runtimePath = new RuntimePath(directory);
		List<RuntimeDefinition> runtimeDefinitions = RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(runtimePath);
		if (runtimeDefinitions.size() == 0) {
			openRuntimeNotFoundMessage();
		} else if (runtimeDefinitions.size() > 1) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					RuntimeUIActivator.launchSearchRuntimePathDialog(
							Display.getDefault().getActiveShell(),
							RuntimeUIActivator.getDefault().getModel().getRuntimePaths(), false, 7);
				}
			});
		} else /* size == 1 */{
			RuntimeInitializerUtil.initializeRuntimes(runtimeDefinitions);
		}
	}

	private static void openRuntimeNotFoundMessage() {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(Display.getDefault()
						.getActiveShell(), "Error", "No runtime/server found...");//$NON-NLS-1$
			}
		});
	}
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		showDecorations();
	}

}
