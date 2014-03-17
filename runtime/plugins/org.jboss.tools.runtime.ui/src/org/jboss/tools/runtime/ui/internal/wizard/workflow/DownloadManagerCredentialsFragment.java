/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/

package org.jboss.tools.runtime.ui.internal.wizard.workflow;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * This page will request credentials from the user to be used during the 
 * download of the file. It is customized for urls that are part of the download
 * manager api, but will work for other urls that simply redirect
 * to a file. 
 * 
 * It will first make a request without any xml headers. In the event 
 * of an error condition, it will try to use the xml header to know
 * what the next step is, for example, signing the terms and conditions.
 */
public class DownloadManagerCredentialsFragment extends WizardFragment {

	/**
	 * In the event of a workflow error condition (more steps to complete), 
	 * the result of the api call will be stored in the task manager under this key.
	 */
	public static final String WORKFLOW_NEXT_STEP_KEY = "WORKFLOW_NEXT_STEP_KEY";
	
	
	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection"; //$NON-NLS-1$
	private IDialogSettings downloadRuntimeSection;
	private IWizardHandle handle;
	private Text userText, passText;
	private int headerResponse = -1;
	private WizardFragment nextWorkflowFragment = null;
	
	public DownloadManagerCredentialsFragment() {
		IDialogSettings dialogSettings = RuntimeUIActivator.getDefault().getDialogSettings();
		downloadRuntimeSection = dialogSettings.getSection(DOWNLOAD_RUNTIME_SECTION);
		if (downloadRuntimeSection == null) {
			downloadRuntimeSection = dialogSettings.addNewSection(DOWNLOAD_RUNTIME_SECTION);
		}
	}
	
	@Override
	public boolean hasComposite() {
		return true;
	}

	@Override
	protected void createChildFragments(List<WizardFragment> list) {
		if( nextWorkflowFragment != null ) {
			list.add(nextWorkflowFragment);
		}
	}

	@Override
	public void enter() {
		setComplete(false);
	}
	
	@Override
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.handle = handle;
		getPage().setTitle("JBoss.org Credentials");
		getPage().setDescription("Please use your jboss.org single sign-on credentials to begin your download.");

		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		contents.setLayoutData(gd);
		contents.setLayout(new GridLayout(1, false));
		
		Composite wrap = new Composite(contents, SWT.NONE);
		wrap.setLayout(new GridLayout(2,  false));
		
		Label l = new Label(wrap, SWT.None);
		l.setText("Please enter your jboss.org credentials below.");
		GridData gd1 = new GridData();
		gd1.horizontalSpan = 2;
		l.setLayoutData(gd1);
		
		Label userLabel = new Label(wrap, SWT.NONE);
		userLabel.setText("Username: ");
		userText = new Text(wrap, SWT.BORDER | SWT.BORDER);
		Label passLabel = new Label(wrap, SWT.NONE);
		passLabel.setText("Password: ");
		passText = new Text(wrap, SWT.PASSWORD | SWT.BORDER);
		userText.setText("");
		userText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				getTaskModel().putObject(DownloadRuntimesTaskWizard.USERNAME_KEY, userText.getText());
				headerResponse = -1;
				setComplete(false);
				DownloadManagerCredentialsFragment.this.handle.update();
			}
		});
		passText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				getTaskModel().putObject(DownloadRuntimesTaskWizard.PASSWORD_KEY, passText.getText());
				headerResponse = -1;
				setComplete(false);
				DownloadManagerCredentialsFragment.this.handle.update();
			}
		});
		
		GridData gd2 = new GridData();
		gd2.widthHint = 200;
		userText.setLayoutData(gd2);
		passText.setLayoutData(gd2);

		
		
		Button b= new Button(wrap, SWT.PUSH);
		b.setText("Validate Credentials");
		b.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				validationPressed();
			}
		});
		
		wrap.setLayoutData(gd);
		
		
		
		return contents;
	}

	public void finishPage() {
		// TODO
	}

	private void validationPressed() {
		final String userS = userText.getText();
		final String passS = passText.getText();
		setComplete(false);
		final String[] error = new String[1];
		try {
			handle.run(true, false, new IRunnableWithProgress() {
				public void run(IProgressMonitor monitor) throws InvocationTargetException,
						InterruptedException {
					error[0] = validateCredentials(userS, passS);
				}
			});
			
			if( error[0] != null )
				handle.setMessage(error[0], IWizardHandle.ERROR);
			else
				handle.setMessage(null, IWizardHandle.NONE);
			setComplete(error[0] == null);
			handle.update();
		} catch(InterruptedException ie) {
			RuntimeUIActivator.pluginLog().logError(ie);
		} catch(InvocationTargetException ite) {
			RuntimeUIActivator.pluginLog().logError(ite);
		}
	}
	
	/**
	 * If the URL is from download-manager/rest/jdf, then the results vary depending on whether 
	 * an xml header has been added or not. 
	 *    If an XML header is NOT added to the request:
	 *       1) invalid credentials return status 401
	 *       2) valid credentials but invalid workflow return 403
	 *       3) everything ok returns 302 redirect to the file
	 *       
	 *    If xml header is added to the request:
	 *       1) invalid credentials return status 401
	 *       2) valid credentials return status 200 with xml content indicating what's missing
	 *       3) everything ok returns 302 redirect to the file
	 *       
	 * @param userS
	 * @param passS
	 * @return an error message
	 */
		
	private String validateCredentials(String userS, String passS) {
		try {
			int response = DownloadManagerWorkflowUtility.getWorkflowStatus(getDownloadRuntimeFromTaskModel(), userS, passS);
			if( response == DownloadManagerWorkflowUtility.CREDENTIALS_FAILED ) {
				return "Your credentials are incorrect. Please review the values and try again.";
			} else if( response == DownloadManagerWorkflowUtility.WORKFLOW_FAILED ) {
				String responseContent = DownloadManagerWorkflowUtility.getWorkflowResponseContent(
						getDownloadRuntimeFromTaskModel(), userS, passS);
				getTaskModel().putObject(WORKFLOW_NEXT_STEP_KEY, responseContent);
				nextWorkflowFragment = DownloadManagerWorkflowUtility.getNextWorkflowFragment(responseContent);
				if( nextWorkflowFragment == null ) {
					// we got a workflow error, but no fragment available. 
					// Point the user to a hyperlink in web browser  TODO
					return "Unable to display the next workflow step. To download, please visit " + getDownloadRuntimeFromTaskModel().getUrl();
				} else {
					return null;
				}
				// need to find the next path
			} else if( response == DownloadManagerWorkflowUtility.AUTHORIZED ) {
				// this page is complete
				nextWorkflowFragment = null;
				return null;
			}
		} catch(Exception e) {
			RuntimeUIActivator.pluginLog().logError(e);
			return e.getMessage();
		}
		return null;
	}
	
	private void threadSafeSetPageComplete(final boolean b) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				setComplete(b);
				handle.update();
			}
		});
	}

	private DownloadRuntime getDownloadRuntimeFromTaskModel() {
		return (DownloadRuntime)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP);
	}
}
