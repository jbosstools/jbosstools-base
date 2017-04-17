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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.foundation.core.xml.IMemento;
import org.jboss.tools.foundation.core.xml.XMLMemento;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.DownloadRuntimeMessages;
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
public class DownloadManagerTermsAndConditionsFragment extends WizardFragment {

	/**
	 * In the event of an error condition, the result of the api call
	 * will be stored in the task manager under this key.
	 */
	public static final String WORKFLOW_NEXT_STEP_KEY = "WORKFLOW_NEXT_STEP_KEY_TC"; //$NON-NLS-1$
	
	
	private static final String DOWNLOAD_RUNTIME_SECTION = "downloadRuntimeSection"; //$NON-NLS-1$
	private IDialogSettings downloadRuntimeSection;
	private IWizardHandle handle;
	private WizardFragment nextWorkflowFragment = null;
	
	// data from the toc rest api
	private String tocText;
	private String tcUrl;
	private HashMap<String, String> countryMap = null; // Not sure if k/v will ever change, so for now we'll use a map
	private ArrayList<String> countryList = null;
	private boolean initialized = false;
	private Browser browser;
	private Button retry;
	
	public DownloadManagerTermsAndConditionsFragment() {
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
		if( !initialized ) {
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					deferredEntry();
				}
			});
		}
	}
	
	private void deferredEntry() {
		try {
			handle.run(true, false, new IRunnableWithProgress() {
				@Override
				public void run(IProgressMonitor monitor) throws InvocationTargetException,
						InterruptedException {
					initializeModel(monitor);
				}
			});
			fillWidgets();
			initialized = true;
			handle.update();
		} catch(Exception e) {
			RuntimeUIActivator.pluginLog().logError(e);
		}
	}
	
	protected void initializeModel(IProgressMonitor monitor) {
		monitor.beginTask("Loading Terms and Conditions", 1000);
		
		String workflowResponse = (String)getTaskModel().getObject(DownloadManagerCredentialsFragment.WORKFLOW_NEXT_STEP_KEY);
		if( workflowResponse == null || workflowResponse.isEmpty()) {
			Display.getDefault().asyncExec(new Runnable() { public void run() {
				Exception e = new Exception("Unable to locate url for downloading runtime: Rest API response is empty.");
				RuntimeCoreActivator.pluginLog().logError(e.getMessage(), e);
				handle.setMessage(e.getMessage(), IWizardHandle.ERROR);
			}});
			return;
		} 
		XMLMemento m = XMLMemento.createReadRoot(new ByteArrayInputStream(workflowResponse.getBytes()));
		IMemento workflow = m.getChild("workflow");
		IMemento tc = workflow.getChild("tc");
		IMemento tcAccept = workflow.getChild("tc-accept");
		this.tcUrl = ((XMLMemento)tc).getTextData();
//		this.tcAcceptUrl = ((XMLMemento)tcAccept).getTextData();
		monitor.worked(100);
		
		// Long-running remote request
		String tcResponseString = null;
		try {
			tcResponseString = getTCResponseString(new SubProgressMonitor(monitor, 800));
		} catch(Exception e) {
			final String msg = "An error occurred while loading the terms and conditions: " + e.getClass().getName() + " - " + e.getMessage();
			RuntimeCoreActivator.pluginLog().logError(msg, e);
			Display.getDefault().asyncExec(new Runnable() { public void run() {
				handle.setMessage(msg, IWizardHandle.ERROR);
			}});
			return;
		}
		IMemento tocResponseMemento = XMLMemento.createReadRoot(new ByteArrayInputStream(tcResponseString.getBytes()));
		
		// Get the options for country
		IMemento customParamsMemento = tocResponseMemento.getChild("customParams");
		IMemento[] params = customParamsMemento.getChildren("param");
		
		IMemento countryParam = null;
		IMemento downloadURLParam = null;
		for( int i = 0; i < params.length; i++ ) {
			String name = params[i].getString("name"); 
			if( name.equals("country")) {
				countryParam = params[i];
			}
			if( name.equals("downloadURL")) {
				downloadURLParam = params[i];
			}
		}
		
		
		IMemento countryOptions = countryParam.getChild("options");
		IMemento[] countries = countryOptions.getChildren("option");
		countryMap = new HashMap<String, String>();
		countryList = new ArrayList<String>();
		for( int i = 0; i < countries.length; i++ ) {
			countryList.add(countries[i].getString("key"));
			countryMap.put(countries[i].getString("key"), countries[i].getString("value"));
		}
		
		IMemento downloadURLOptions = downloadURLParam.getChild("options");
		IMemento downloadURLOption = downloadURLOptions.getChild("option");
//		downloadURL = downloadURLOption.getString("key");
		
		// get the TC plaintext
		IMemento tcPlainTextMemento = tocResponseMemento.getChild("htmlText");
		String plaintext = ((XMLMemento)tcPlainTextMemento).getTextData();
		tocText = plaintext;
		monitor.worked(100);
		monitor.done();
	}
	
	private void fillWidgets() {
		if( browser != null && !browser.isDisposed() && tocText != null) {
			browser.setText(tocText);
		}
	}
	
	/*
	 * Long running task to get the terms and conditions
	 */
	private String getTCResponseString(IProgressMonitor monitor) throws Exception {
        String result = "";
   		try {
			// Now we need to fetch the terms and conditions
			HttpURLConnection con =
					(HttpURLConnection) new URL(tcUrl).openConnection();
			con.setRequestMethod("GET");

			// We need to get the content of this response to see what the next step is
	        InputStream stream = con.getInputStream();

	        BufferedReader br = new BufferedReader(new InputStreamReader(stream));
	        String line;
	        while ((line = br.readLine()) != null) {
		        result+= line;
	        }

	        con.disconnect();
	        br.close();
		} catch(Exception e) {
			throw e;
		}
   		return result;
	}
	
	@Override
	public Composite createComposite(Composite parent, final IWizardHandle handle) {
		this.handle = handle;
		getPage().setTitle("JBoss.org Terms and Conditions");
		getPage().setDescription("Please use the link below to sign the terms and conditions. Once complete, please press 'Retry' to continue.");

		Composite contents = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_BOTH);
		contents.setLayoutData(gd);
		contents.setLayout(new FormLayout());
		
		try {
			browser = new Browser(contents, SWT.BORDER);
		} catch (Exception e1) {
			browser = new Browser(contents, SWT.BORDER | SWT.WEBKIT);
		}
		
		retry = new Button(contents, SWT.PUSH);
		retry.setText("Retry");
		retry.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				retryPressed();
			}
		});
		
		FormData fd = new FormData();
		fd.bottom = new FormAttachment(0, 400);
		fd.left = new FormAttachment(0, 5);
		fd.right = new FormAttachment(100, -5);
		fd.top = new FormAttachment(0,5);
		browser.setLayoutData(fd);
		
		fd = new FormData();
		fd.top = new FormAttachment(browser, 10);
		fd.left = new FormAttachment(0,5);
		retry.setLayoutData(fd);
		
		setComplete(false);
		fillWidgets();
		return contents;
	}

	private DownloadRuntime getDownloadRuntimeFromTaskModel() {
		return (DownloadRuntime)getTaskModel().getObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP);
	}
	
	protected void retryPressed() {
		final String userS = (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.USERNAME_KEY);
		final String passS = (String)getTaskModel().getObject(DownloadRuntimesTaskWizard.PASSWORD_KEY);
		String errString = null;
		try {
			int response = DownloadManagerWorkflowUtility.getWorkflowStatus(getDownloadRuntimeFromTaskModel(), userS, passS);
		if( response == DownloadManagerWorkflowUtility.CREDENTIALS_FAILED ) {
			errString = DownloadRuntimeMessages.CredentialsIncorrect;
		} else if( response == DownloadManagerWorkflowUtility.WORKFLOW_FAILED ) {
			errString = "Your authorization process is still incomplete. Please press 'back' and try again.";
		} else if( response == DownloadManagerWorkflowUtility.AUTHORIZED ) {
			setComplete(true);
			handle.update();
		}
			
		} catch(IOException e) {
			RuntimeUIActivator.pluginLog().logError(e);
			errString = NLS.bind(DownloadRuntimeMessages.CredentialError, e.getClass().getName(), e.getMessage());
		} catch(CoreException ce) {
			RuntimeUIActivator.pluginLog().logError(ce);
			errString = NLS.bind(DownloadRuntimeMessages.CredentialError, ce.getClass().getName(), ce.getMessage());
		}
		if( errString != null ) {
			setComplete(false);
			handle.setMessage(errString, IMessageProvider.ERROR);
			handle.update();
		}
	}
	
	public void finishPage() {
		// TODO
	}
}
