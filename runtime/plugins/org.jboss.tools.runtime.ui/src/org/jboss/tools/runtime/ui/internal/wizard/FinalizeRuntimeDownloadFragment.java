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
import org.eclipse.ui.progress.IProgressService;
import org.jboss.tools.foundation.core.jobs.DelegatingProgressMonitor;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class FinalizeRuntimeDownloadFragment extends WizardFragment {

	private static final String FOLDER_IS_REQUIRED = Messages.DownloadRuntimesSecondPage_This_folder_is_required;
	private static final String FOLDER_IS_NOT_WRITABLE = Messages.DownloadRuntimesSecondPage_This_folder_does_not_exist;
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
	private ControlDecoration decPathError;
	private ControlDecoration decPathReq;
	private ControlDecoration destinationPathError;
	private ControlDecoration destinationPathReq;
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
		decPathError = addDecoration(pathText, FieldDecorationRegistry.DEC_WARNING, FOLDER_IS_NOT_WRITABLE);
		decPathReq = addDecoration(pathText, FieldDecorationRegistry.DEC_REQUIRED, FOLDER_IS_REQUIRED);
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
		destinationPathError = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_WARNING, FOLDER_IS_NOT_WRITABLE);
		destinationPathReq = addDecoration(destinationPathText, FieldDecorationRegistry.DEC_REQUIRED, FOLDER_IS_REQUIRED);
		
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
		decPathError.hide();
		decPathReq.hide();
		destinationPathError.hide();
		destinationPathReq.hide();
		
		if (path.isEmpty()) {
			decPathReq.show();
		}
		if (destination.isEmpty()) {
			destinationPathReq.show();
		}
		boolean pathExists = checkPath(path, decPathError);
		boolean destExists = checkPath(destination, destinationPathError);
		String msg = null;
		int msgType = IWizardHandle.ERROR;
		if (!pathExists) {
			msg = Messages.DownloadRuntimesSecondPage_Install_folder_does_not_exist;
			msgType = IWizardHandle.WARNING;
		} else if (path.isEmpty()) {
			msg = Messages.DownloadRuntimesSecondPage_Install_folder_is_required;
		} else if (!destExists) {
			msg = Messages.DownloadRuntimesSecondPage_19;
			msgType = IWizardHandle.INFORMATION;
		} else if (destination.isEmpty()) {
			msg = Messages.DownloadRuntimesSecondPage_Download_folder_is_required;
		}
		decPathError.setShowHover(true);
		if( msg != null )
			handle.setMessage(msg, msgType);
		setComplete(msg == null || msgType != IWizardHandle.ERROR);
		handle.update();
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
		dialogSettings.put(DEFAULT_DESTINATION_PATH,
				destinationPathText.getText());
		dialogSettings.put(DEFAULT_DIALOG_PATH, pathText.getText());
		dialogSettings.put(DELETE_ON_EXIT, delete);
	}
	
	private boolean downloadRuntime(final String selectedDirectory,
			final String destinationDirectory, final boolean deleteOnExit, IProgressMonitor monitor) {
		saveDialogSettings();
		final DownloadRuntime downloadRuntime = getDownloadRuntimeFromTaskModel();
		final boolean suppressCreation = shouldSuppressCreation();
		final DelegatingProgressMonitor delegatingMonitor = new DelegatingProgressMonitor();
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DOWNLOAD_JOB_DELEGATING_PROGRESS_MONITOR, delegatingMonitor);
		
		Job downloadJob = new Job("Download '" + downloadRuntime.getName()) {//$NON-NLS-1$

			@Override
			public IStatus run(IProgressMonitor jobMonitor) {
				String user = (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.USERNAME_KEY);
				String pass = (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.PASSWORD_KEY);
				delegatingMonitor.add(jobMonitor);
				delegatingMonitor.beginTask("Download '" + downloadRuntime.getName() + "' ...", 100);//$NON-NLS-1$ //$NON-NLS-2$
				delegatingMonitor.worked(1);
				IStatus ret = null;
				if( !suppressCreation ) {
					ret = new DownloadRuntimeOperationUtility().downloadAndInstall(selectedDirectory, destinationDirectory, 
						getDownloadUrl(), deleteOnExit, user, pass, getTaskModel(), new SubProgressMonitor(delegatingMonitor, 99));
				} else {
					ret = new DownloadRuntimeOperationUtility().downloadAndUnzip(selectedDirectory, destinationDirectory, 
							getDownloadUrl(), deleteOnExit, user, pass, getTaskModel(), new SubProgressMonitor(delegatingMonitor, 99));
				}
				
				if( delegatingMonitor.isCanceled()) 
					return Status.CANCEL_STATUS;
				if( !ret.isOK()) {
					openErrorMessage(ret.getMessage());
				}
				return Status.OK_STATUS;
			}
		};
		downloadJob.setUser(false);
		downloadJob.schedule(1500);
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DOWNLOAD_JOB, downloadJob);
		IProgressService progressService= PlatformUI.getWorkbench().getProgressService();
		progressService.showInDialog(getActiveShell(), downloadJob);
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
	
	private static void openErrorMessage(final String title, final String msg, boolean log) {
		if( log )
			RuntimeUIActivator.pluginLog().logError(msg);
		openErrorMessage("Error", msg); //$NON-NLS-1$
	}
	
	private static void openErrorMessage(final String title, final String msg) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(Display.getDefault()
						.getActiveShell(), title, msg);
			}
		});		
	}

	
}
