/*************************************************************************************
 * Copyright (c) 2010-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
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
import org.jboss.tools.foundation.core.jobs.DelegatingProgressMonitor;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.extract.IOverwrite;
import org.jboss.tools.runtime.core.internal.RuntimeExtensionManager;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeWorkflowConstants;
import org.jboss.tools.runtime.core.model.IRuntimeInstaller;
import org.jboss.tools.runtime.core.util.internal.DownloadRuntimeOperationUtility;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class FinalizeRuntimeDownloadFragment extends WizardFragment {
	public static final String FINALIZE_RUNTIMED_OWNLOAD_FRAGMENT_INSTALLPATH = "FinalizeRuntimeDownloadFragment.installPath";
	
	
	
	private static final String DELETE_ON_EXIT = "deleteOnExit"; //$NON-NLS-1$
	private static final String JAVA_IO_TMPDIR = "java.io.tmpdir"; //$NON-NLS-1$
	private static final String USER_HOME = "user.home"; //$NON-NLS-1$
	private static final String DEFAULT_DIALOG_PATH = "defaultDialogPath"; //$NON-NLS-1$
		
	private static final String DEFAULT_DESTINATION_PATH = "defaultDestinationPath"; //$NON-NLS-1$

	private DownloadRuntime dlrt;

	private IDialogSettings dialogSettings;
	private Button deleteOnExit;
	private Text destinationPathText;
	private Text pathText;
	private String delete;
	private ControlDecoration decPathError_dne, decPathError_writable, decPathReq;
	private ControlDecoration destinationPathError_dne, destinationPathError_writable, destinationPathReq;
	private Link urlText;
	private Group warningComposite;
	private Label warningLabel;
	private Link warningLink;
	private Composite contents;
	private Composite pathComposite;
	private Shell shell;
	
	private IWizardHandle handle;
	
	public FinalizeRuntimeDownloadFragment() {
		dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
	}

	@Override
	public boolean hasComposite() {
		return true;
	}

	@Override
	public void enter() {
		DownloadRuntime tmp = getDownloadRuntimeFromTaskModel();
		if( tmp != null && !tmp.equals(dlrt)) {
			dlrt = tmp;
			setDownloadRuntime(dlrt);
		}
	}
	
	private DownloadRuntime getDownloadRuntimeFromTaskModel() {
		return (DownloadRuntime)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP);
	}
	
	private boolean shouldSuppressCreation() {
		Object suppress = getTaskModel().getObject(DownloadRuntimesTaskWizard.SUPPRESS_RUNTIME_CREATION);
		if( suppress instanceof Boolean ) {
			return ((Boolean)suppress).booleanValue();
		}
		return false;
	}
	
	private void setDescription() {
		if( getPage() == null )
			return;
		
		getPage().setTitle(Messages.DownloadRuntimesSecondPage_Download_Runtime);
		if (getDownloadRuntimeFromTaskModel() != null) {
			getPage().setDescription("Download Runtime '" + getDownloadRuntimeFromTaskModel().getName() + "'");//$NON-NLS-1$ //$NON-NLS-2$
		} else {
			getPage().setDescription("Download Runtime");//$NON-NLS-1$
		}
	}

	@Override
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.handle = handle;
		this.shell = parent.getShell();
		
		contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
				
		pathComposite = createAndFillPathComposite(contents);

		setDownloadRuntime((DownloadRuntime)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP));
		setDescription();
		refresh();
		return contents;
	}

	private Composite createAndFillPathComposite(Composite parent) {
		Composite pathComposite = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathComposite.setLayoutData(gd);
		pathComposite.setLayout(new GridLayout(3, false));
		
		Label urlLabel = new Label(pathComposite, SWT.NONE);
		urlLabel.setText(Messages.DownloadRuntimesSecondPage_URL);
		urlText = new Link(pathComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		gd.horizontalSpan=2;
		urlText.setLayoutData(gd);
		urlText.addSelectionListener( new SelectionAdapter( ) {

			public void widgetSelected( SelectionEvent e )
			{
				String t = e.text;
				String humanUrl = getDownloadRuntimeFromTaskModel().getHumanUrl();
				if (humanUrl != null && t.contains(humanUrl)) {
					IWorkbenchBrowserSupport support = PlatformUI.getWorkbench()
							.getBrowserSupport();
					try {
						URL url = new URL(humanUrl);
						support.getExternalBrowser().openURL(url);
					} catch (Exception e1) {
						RuntimeUIActivator.pluginLog().logError(e1);
						final String message = e1.getMessage();
						Display.getDefault().syncExec(new Runnable() {

							@Override
							public void run() {
								MessageDialog.openError(getActiveShell(), "Error", message);//$NON-NLS-1$
							}
							
						});
					}
				}
				
			}
		} );
		
		if( requiresManualDownload()) {
			// Do not make the remainder of the widgets
			return pathComposite;
		}

		Label pathLabel = new Label(pathComposite, SWT.NONE);
		pathLabel.setText(Messages.DownloadRuntimesSecondPage_Install_folder);
		
		pathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		pathText.setLayoutData(gd);
		final String defaultPath = getDefaultPath();
		pathText.setText(defaultPath);
		decPathError_dne = addDecoration(pathText, FieldDecorationRegistry.DEC_WARNING, Messages.DownloadRuntimesSecondPage_This_folder_does_not_exist);
		decPathError_writable = addDecoration(pathText, FieldDecorationRegistry.DEC_ERROR, Messages.DownloadRuntimesSecondPage_This_folder_not_writable);
		decPathReq = addDecoration(pathText, FieldDecorationRegistry.DEC_REQUIRED, Messages.DownloadRuntimesSecondPage_This_folder_is_required);

		pathText.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});
		
		Button browseButton = new Button(pathComposite, SWT.NONE);
		browseButton.setText(Messages.DownloadRuntimesSecondPage_Browse);
		browseButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(shell);
				dialog.setMessage(Messages.DownloadRuntimesSecondPage_Select_install_folder);
				dialog.setFilterPath(pathText.getText());
				final String path = dialog.open();
				if (path == null) {
					return;
				}
				pathText.setText(path);
			}
		
		});
		
		Label destinationLabel = new Label(pathComposite, SWT.NONE);
		destinationLabel.setText(Messages.DownloadRuntimesSecondPage_Download_folder);
		
		destinationPathText = new Text(pathComposite, SWT.BORDER);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		destinationPathText.setLayoutData(gd);
		String destinationPath = dialogSettings.get(DEFAULT_DESTINATION_PATH);
		
		destinationPathError_dne = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_WARNING, Messages.DownloadRuntimesSecondPage_This_folder_does_not_exist);
		destinationPathError_writable = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_ERROR, Messages.DownloadRuntimesSecondPage_This_folder_not_writable);
		destinationPathReq = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_REQUIRED, Messages.DownloadRuntimesSecondPage_This_folder_is_required);
		
		if (destinationPath == null || destinationPath.isEmpty()) {
			destinationPath=System.getProperty(JAVA_IO_TMPDIR);
		}
		destinationPathText.setText(destinationPath);
		Button browseDestinationButton = new Button(pathComposite, SWT.NONE);
		browseDestinationButton.setText(Messages.DownloadRuntimesSecondPage_Browse);
		browseDestinationButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(shell);
				dialog.setMessage(Messages.DownloadRuntimesSecondPage_Select_ownload_folder);
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
		deleteOnExit.setText(Messages.DownloadRuntimesSecondPage_Delete_archive_after_installing);
		
		delete = dialogSettings.get(DELETE_ON_EXIT);
		if (delete == null) {
			delete = Boolean.TRUE.toString();
		}
		deleteOnExit.setSelection(new Boolean(delete));
		deleteOnExit.addSelectionListener(new SelectionAdapter() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				delete = new Boolean(deleteOnExit.getSelection()).toString();
			}
		});
		
		return pathComposite;
	}
	
	private Group createWarningComposite(Composite parent) {
		warningComposite = new Group(parent, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		//gd.horizontalSpan = 3;
		warningComposite.setLayoutData(gd);
		warningComposite.setLayout(new GridLayout(1, false));
		warningComposite.setText(Messages.DownloadRuntimesSecondPage_Warning);
		
		warningLabel = new Label(warningComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		warningLabel.setLayoutData(gd);
		warningLink = new Link(warningComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		warningLink.setLayoutData(gd);
		
		warningLink.addSelectionListener( new SelectionAdapter( ) {

			public void widgetSelected( SelectionEvent e )
			{
				String text = e.text;
				String humanUrl = getDownloadRuntimeFromTaskModel() == null ? null : getDownloadRuntimeFromTaskModel().getHumanUrl();
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
						RuntimeUIActivator.pluginLog().logError(e1);
						final String message = e1.getMessage();
						Display.getDefault().syncExec(new Runnable() {

							@Override
							public void run() {
								MessageDialog.openError(getActiveShell(), "Error", message);//$NON-NLS-1$
							}
							
						});
					}
				}
				
			}
		} );
		return warningComposite;
	}

	private String getDefaultPath() {
		String fromModel = (String)getTaskModel().getObject(FINALIZE_RUNTIMED_OWNLOAD_FRAGMENT_INSTALLPATH);
		if( fromModel != null && !fromModel.isEmpty()) {
			return fromModel;
		}
		String defaultPath = dialogSettings.get(DEFAULT_DIALOG_PATH);
		if (defaultPath == null || defaultPath.isEmpty()) {
			defaultPath=System.getProperty(USER_HOME);
		}
		return defaultPath;
	}

	private void showDecorations() {
		if( requiresManualDownload() )
			return;
		
		String path = pathText == null || pathText.isDisposed() ? "" : pathText.getText();
		String destination = destinationPathText == null || destinationPathText.isDisposed() ? "" : destinationPathText.getText();
		
		// hide all decorations
		decPathError_dne.hide();
		decPathError_writable.hide();
		decPathReq.hide();
		destinationPathError_dne.hide();
		destinationPathError_writable.hide();
		destinationPathReq.hide();
		
		ArrayList<IStatus> all = new ArrayList<IStatus>();
		if( path.isEmpty() ) {
			decPathReq.show();
			all.add(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Install_folder_is_required));
		} 
		if( destination.isEmpty() ) {
			destinationPathReq.show();
			all.add(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Download_folder_is_required));
		} 
		boolean pathExists = checkPathExists(path);
		boolean pathWritable = checkPathWritable(path); //, decPathError);
		boolean destExists = checkPathExists(destination);
		boolean destWritable = checkPathWritable(destination);  //, destinationPathError);
		
		
		if( pathExists && !pathWritable ) {
			decPathError_writable.show();
			decPathError_writable.setShowHover(true);
			all.add(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Install_folder_not_writable));
		} 
		if( destExists && !destWritable ) {
			destinationPathError_writable.show();
			destinationPathError_writable.setShowHover(true);
			all.add(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Download_folder_not_writable));
		}
		if (!pathExists) {
			decPathError_dne.show();
			decPathError_dne.setShowHover(true);
			all.add(new Status(IStatus.INFO, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Install_folder_does_not_exist));
		} 
		if( !destExists) {
			destinationPathError_dne.show();
			destinationPathError_dne.setShowHover(true);
			all.add(new Status(IStatus.INFO, RuntimeUIActivator.PLUGIN_ID, Messages.DownloadRuntimesSecondPage_Download_folder_does_not_exist));
		} 
		
		
		IStatus working = null;
		IStatus worstStatus = null;
		for(Iterator<IStatus> i = all.iterator(); i.hasNext(); ) {
			working = i.next();
			if( worstStatus == null || working.getSeverity() > worstStatus.getSeverity() ) {
				worstStatus = working;
			}
		}
		
		int messageType = IWizardHandle.NONE;
		if( worstStatus != null ) {
			switch(worstStatus.getSeverity()) {
			case IStatus.OK:
				messageType = IWizardHandle.NONE;
				break;
			case IStatus.INFO:
				messageType = IWizardHandle.INFORMATION;
				break;
			case IStatus.WARNING:
				messageType = IWizardHandle.WARNING;
				break;
			case IStatus.ERROR:
				messageType = IWizardHandle.ERROR;
				break;
			
			}
		}
		handle.setMessage(worstStatus == null ? "" : worstStatus.getMessage(), messageType);
		setComplete(messageType != IWizardHandle.ERROR);
		handle.update();
	}
	
	private boolean checkPathExists(String path) {
		if (path.isEmpty()) {
			return true;
		}
		return new File(path).exists();
	}

	private boolean checkPathWritable(String path) {
		if (path.isEmpty()) {
			return true;
		}
		try {
			File file = File.createTempFile("temp", "txt", new File(path));//$NON-NLS-1$ //$NON-NLS-2$
			file.deleteOnExit();
			file.delete();
		} catch (IOException e) {
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
		decPath.hide();
		return decPath;
	}

	protected void validate() {
		handle.setMessage(null, IWizardHandle.NONE);
		showDecorations();
	}

	private void refresh() {
		// Completely remove and then re-create the warning composite (??)
		if (contents != null && !contents.isDisposed()) {
			if( pathComposite != null ) {
				pathComposite.dispose();
				pathComposite = null;
			}
			if (warningComposite != null) {
				warningComposite.dispose();
				warningComposite = null;
			}
			contents.layout(true, true);
			contents.pack();
		}
		
		DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		
		if (downloadRuntime != null) {
			boolean requireManualDownload = requiresManualDownload();
			pathComposite = createAndFillPathComposite(contents);
			if (getDownloadUrl() != null) {
				urlText.setText(getDownloadUrl());
			} else if (downloadRuntime.getHumanUrl() != null){
				urlText.setText("<a>"+downloadRuntime.getHumanUrl().trim()+"</a>");//$NON-NLS-1$ //$NON-NLS-2$
			} else {
				urlText.setText(""); //$NON-NLS-1$
			}
			boolean isDisclaimer = downloadRuntime.isDisclaimer();
			boolean showWarning = isDisclaimer || requireManualDownload;
			
			if (showWarning) {
				warningComposite = createWarningComposite(contents);
				if (isDisclaimer) {
					warningLabel.setText("This is a community project and, as such is not supported with an SLA.");//$NON-NLS-1$
					warningLink.setText("If you're looking for fully supported, certified, enterprise middleware try JBoss Enterprise Middleware products. <a>Show Details</a>");//$NON-NLS-1$
				} else if (requireManualDownload) {
					warningLabel.setText("This runtime is only available as manual download.");//$NON-NLS-1$
					warningLink.setText("Please click on the download <a>link</a>.");//$NON-NLS-1$
				}
			}
		} 
		
		contents.getParent().layout(true, true);
		validate();
		updateWidgetEnablementForDownloadRuntime();
	}

	public void setDownloadRuntime(DownloadRuntime selectedRuntime) {
		setDescription();
		if (contents != null && !contents.isDisposed()) {
			refresh();
			if (selectedRuntime != null) {
				setComplete(true);
			} else {
				setComplete(true);
			}
		} else {
			setComplete(false);
		}
		handle.update();
	}
	
	private void updateWidgetEnablementForDownloadRuntime() {
		DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		if( downloadRuntime == null )
			return;
		if( !requiresManualDownload()) {
			boolean enabled = (getDownloadUrl() != null);
			deleteOnExit.setEnabled(enabled);
			destinationPathText.setEnabled(enabled);
			pathText.setEnabled(enabled);
		}
	}

	private boolean requiresManualDownload() {
		DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		if( downloadRuntime != null ) {
			boolean hasNoDlUrl = downloadRuntime.getUrl() == null && downloadRuntime.getHumanUrl() != null;
			if( hasNoDlUrl ) {
				String fromTaskModel = (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_URL);
				// No authenticator UI has overridden this with a new / temporary URL
				if( fromTaskModel == null )
					return true;
			}
			// Either the downloadruntime includes all needed information, or 
			// the taskModel contains the information
		}
		return false;
	}
	
	private String getDownloadUrl() {
		DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		if( downloadRuntime != null ) {
			String dlUrl = downloadRuntime.getUrl();
			if( dlUrl == null ) {
				return (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_URL);
			}
			return dlUrl;
		}
		return null;
	}
	
	public void performFinish(final IProgressMonitor monitor) {
		Display.getDefault().syncExec(new Runnable() { 
			public void run() {
				finishPage(monitor);
			}
		});
	}
	
	public boolean finishPage(IProgressMonitor monitor) {
		if( requiresManualDownload() ) {
			// User is expected to have downloaded this on their own
			return true;
		}
		
		String selectedDirectory = pathText.getText();
		String destinationDirectory = destinationPathText.getText();
		boolean del = deleteOnExit.getSelection();
		return downloadRuntime(selectedDirectory, destinationDirectory, del, monitor);
	}

	private void saveDialogSettings() {
		String systemTempDir = System.getProperty(JAVA_IO_TMPDIR);
		if( systemTempDir.equals(destinationPathText.getText())) {
			dialogSettings.put(DEFAULT_DESTINATION_PATH, (String)null);
		} else {
			dialogSettings.put(DEFAULT_DESTINATION_PATH,
					destinationPathText.getText());
		}
		dialogSettings.put(DEFAULT_DIALOG_PATH, pathText.getText());
		dialogSettings.put(DELETE_ON_EXIT, delete);
	}
	
	private boolean downloadRuntime(final String unzipDirectory,
			final String downloadDirectory, final boolean deleteOnExit, IProgressMonitor monitor) {
		saveDialogSettings();
		final DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		final boolean suppressCreation = shouldSuppressCreation();
		final DelegatingProgressMonitor delegatingMonitor = new DelegatingProgressMonitor();
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DOWNLOAD_JOB_DELEGATING_PROGRESS_MONITOR, delegatingMonitor);
		
		Job downloadJob = new Job("Download '" + downloadRuntime.getName()) {//$NON-NLS-1$

			@Override
			public IStatus run(IProgressMonitor jobMonitor) {
				delegatingMonitor.add(jobMonitor);
				String installationMethod = downloadRuntime.getInstallationMethod();
				IRuntimeInstaller installer = getInstaller(installationMethod);
				
				if( installer == null ) {
					// Show error and cancel
					return new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "Unable to find an installer with id " + installationMethod);
				}
				
				delegatingMonitor.beginTask("Downloading and Installing Runtime", 100);
				IOverwrite ow = DownloadRuntimeOperationUIUtility.createOverwriteFileQuery();
				getTaskModel().putObject(IDownloadRuntimeWorkflowConstants.OVERWRITE, ow);
				int firstStep = suppressCreation ? 99 : 95;
				IStatus ret = installer.installRuntime(downloadRuntime, unzipDirectory, downloadDirectory, deleteOnExit, getTaskModel(), 
						new SubProgressMonitor(delegatingMonitor, firstStep));
				
				if( !suppressCreation && ret.isOK()) {
					String updatedRuntimeRoot = (String)getTaskModel().getObject(DownloadRuntimesWizard.UNZIPPED_SERVER_HOME_DIRECTORY);
					if( updatedRuntimeRoot == null || !new File(updatedRuntimeRoot).exists()) {
						updatedRuntimeRoot = unzipDirectory;
					}
					
					if( updatedRuntimeRoot == null || !new File(updatedRuntimeRoot).exists()) {
						ret = new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "No runtime found at path " + updatedRuntimeRoot);
					} else {
						ret = DownloadRuntimeOperationUIUtility.createRuntimes(updatedRuntimeRoot, new SubProgressMonitor(delegatingMonitor, 4));
					}
				}
				
				if( delegatingMonitor.isCanceled()) 
					return Status.CANCEL_STATUS;
				if( !ret.isOK() && ret.getSeverity() != IStatus.CANCEL) {
					openErrorMessage(ret.getMessage(), true);
				}
				return Status.OK_STATUS;
			}
			
			private IRuntimeInstaller getInstaller(String installerId) {
				return RuntimeExtensionManager.getDefault().getRuntimeInstaller(installerId); 
			}
			
		};
		downloadJob.setUser(false);
		downloadJob.schedule(1000);
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DOWNLOAD_JOB, downloadJob);
		return true;
	}
	
	private Shell getActiveShell() {
		Display display = Display.getDefault();
		if (display != null) {
			if (shell == null)
				return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		}
		return shell;
	}

	private static void openErrorMessage(final String msg, boolean log) {
		openErrorMessage( "Error", msg, log); //$NON-NLS-1$
	}
	
	private static void openErrorMessage(final String msg) {
		openErrorMessage(msg, false);
	}

	private static void openWarningMessage(final String title, final String msg) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openWarning(Display.getDefault()
						.getActiveShell(), title, msg);
			}
		});		
	}

	private static void openErrorMessage(final String title, final String msg, boolean log) {
		if( log )
			RuntimeUIActivator.pluginLog().logError(msg);
		openErrorMessage("Error", msg); //$NON-NLS-1$
	}
	
	private static void openErrorMessage(final String title, final String msg) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				Shell s = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				MessageDialog.openError(s, title, msg);
			}
		});		
	}

	
}
