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
package org.jboss.tools.common.reporting;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.PostMethod;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.util.HttpUtil;

/**
 * HTTP Utility for bug reporting to RedHat.
 * @author Igels
 */
public class Submit {

	public static String REPORT_DESRIPTION_PARAMETER_NAME = CommonPlugin.getMessage("%reportParameterName");

	private static String URL = CommonPlugin.getMessage("%reportingUrl");
	private static String JOB_NAME = CommonPlugin.getMessage("%reportingJobName");
	private static String ERROR_MESSAGE = CommonPlugin.getMessage("%errorReportingMessage");

	private static Submit INSTANCE = new Submit();

	private Submit() {
		super();
	}

	/**
	 * @return Instance of Submit
	 */
	public static Submit getInstance() {
		return INSTANCE;
	}

	/**
	 * Submit report to RedHat.
	 * @param reportText
	 * @param cleanBuffer
	 */
	public void submit(final String reportText, final boolean cleanBuffer) {
		Job job = new Job(JOB_NAME) {
			public IStatus run(IProgressMonitor monitor) {
				try {
					submitReport(reportText);
				} catch (Exception e) {
					String exceptionMessage = e.getMessage();
					String message = ERROR_MESSAGE;
					if(exceptionMessage!=null && exceptionMessage.trim().length()>0) {
						message = message + ".\r\n" + e.getClass().getName() + ": " + exceptionMessage;
					}
					Status status = new Status(Status.WARNING, CommonPlugin.PLUGIN_ID, Status.WARNING, message, e);
					return status;
				}
				if(cleanBuffer) {
					ProblemReportingHelper.buffer.clean();
				}
				return Status.OK_STATUS;
			}
		};
		job.setUser(true);
		job.schedule();
	}

	private int submitReport(String reportText) throws Exception {
		HttpClient httpClient = HttpUtil.createHttpClient(URL);
		PostMethod httpPost = new PostMethod(URL);
		httpPost.addParameter(REPORT_DESRIPTION_PARAMETER_NAME, reportText);
	    int responseCode = httpClient.executeMethod(httpPost);

	    return responseCode;
	}
}