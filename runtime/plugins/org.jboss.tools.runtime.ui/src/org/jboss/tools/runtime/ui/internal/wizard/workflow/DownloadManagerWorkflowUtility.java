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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.GZIPInputStream;

import org.eclipse.core.internal.preferences.Base64;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.foundation.core.xml.IMemento;
import org.jboss.tools.foundation.core.xml.XMLMemento;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * A utility class for running the remote download-manager header commands
 * to verify if the downloadRuntime is set to be downloaded. 
 */
public class DownloadManagerWorkflowUtility {
	public static final int AUTHORIZED = 1;
	public static final int CREDENTIALS_FAILED = 2;
	public static final int WORKFLOW_FAILED = 3;
	
	
	public static int getWorkflowStatus(DownloadRuntime dr, String userS, String passS) 
			throws CoreException, MalformedURLException, IOException {
		int response = headerOnlyStatusCode(dr, userS, passS);
		if( response == 401 ) {
			// 401 means bad credentials, change nothing
			return CREDENTIALS_FAILED;
		} else if( response == 403 || response == 200) {
			// 403 means workflow incomplete / forbidden, need a child page
			return WORKFLOW_FAILED;
		} else if( response == 302 ) {
			// 302 means all's clear / redirect,  no child page needed
			return AUTHORIZED;
		}
		throw new CoreException(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "Unknown response code: " + response));

	}
	
	private static int headerOnlyStatusCode(DownloadRuntime dr, String userS, String passS)
			throws CoreException, MalformedURLException, IOException {
		HttpURLConnection con = getWorkflowConnection(dr, userS, passS, "HEAD", true);
		int response = con.getResponseCode();
		con.disconnect();
		return response;
	}

	
	// Example curl command:  
	// curl --verbose -L -u user:pass -H "Content-Type: application/xml" -H "Accept: application/xml"  -O https://www.jboss.org/download-manager/jdf/file/jboss-eap-6.3.0.GA.zip

	
	// This is a connection to see where we stand in the workflow
	private static HttpURLConnection getWorkflowConnection(DownloadRuntime dr, 
			String user, String pass, String requestMethod, boolean useXMLHeader) 
			throws IOException, MalformedURLException {
		String url = dr.getUrl();
		HttpURLConnection con =
				(HttpURLConnection) new URL(url).openConnection();
		con.setInstanceFollowRedirects(false);
		String userCredentials = user+ ":" + pass;
		String basicAuth = "Basic " + new String(new Base64().encode(userCredentials.getBytes()));
		con.setRequestProperty ("Authorization", basicAuth);
		if( useXMLHeader ) {
			con.setRequestProperty("Content-Type", "application/xml");
		    con.setRequestProperty("Accept", "application/xml");
		}
		con.setRequestMethod(requestMethod);
		con.setReadTimeout(30000);
		return con;
	}

	private static String findNextStep(String responseContent) {
		if( responseContent != null && !responseContent.isEmpty()) {
			XMLMemento m = XMLMemento.createReadRoot(new ByteArrayInputStream(responseContent.getBytes()));
			if( m != null ) {
				IMemento workflow = m.getChild("workflow");
				IMemento step = workflow.getChild("step");
				String nextStep = ((XMLMemento)step).getTextData();
				return nextStep;
			}
		}
		return null;
	}
	
	/**
	 * Try to acquire the xml response indicating what the next step in the t&c is for this url. 
	 * Due to a bug on the server, the current implementation will check both normal responses,
	 * and a gzip'd response.  If the normal response is properly formatted xml, it will be used. 
	 * If it is not, we will attempt to read the input stream through a gzip input stream, 
	 * to see if that one returns properly formatted xml. 
	 *
	 * @param dr
	 * @param userS
	 * @param passS
	 * @return  A workflow response if it can be found, or null
	 * @throws IOException
	 */
	public static String getWorkflowResponseContent(DownloadRuntime dr, String userS, String passS) throws IOException {
		HttpURLConnection con = getWorkflowConnection(dr, userS, passS, "GET", true);
		
		// We need to get the content of this response to see what the next step is
        InputStream stream = con.getInputStream();
        // As byte array
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        int nRead;
        byte[] data = new byte[16384];
        while ((nRead = stream.read(data, 0, data.length)) != -1) {
          buffer.write(data, 0, nRead);
        }
        buffer.flush();
        byte[] bytes = buffer.toByteArray();
        con.disconnect();
        
        
        
        // Try to read it normally
		String result = "";
        BufferedReader br = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(bytes)));
        String line;
        while ((line = br.readLine()) != null) {
	        result += line;
        }
        br.close();
        
        // Parse the response to see the next step in the process
        String nextStep = findNextStep(result);
        int x;
        
        // If the result was not properly formatted xml, try via gzip
        if( nextStep == null ) {
	        // Try to read it gzip-style, due to bugs on the server: JBIDE-17253
			result = "";
	        br = new BufferedReader(new InputStreamReader(new GZIPInputStream(new ByteArrayInputStream(bytes))));
	        while ((line = br.readLine()) != null) {
		        result += line;
	        }
	        br.close();
        }
        return result;
	}
	
	public static WizardFragment getNextWorkflowFragment(String response) {
		String nextStep = findNextStep(response);
		if( "termsAndConditions".equals(nextStep)) { //$NON-NLS-1$
			// return the tc page
			return new DownloadManagerTermsAndConditionsFragment();
		}
		return null;
	}
}
