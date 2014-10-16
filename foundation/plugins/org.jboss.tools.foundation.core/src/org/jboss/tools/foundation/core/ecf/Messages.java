/******************************************************************************* 
 * Copyright (c) 2009 - 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

package org.jboss.tools.foundation.core.ecf;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String RESOURCE = "org.jboss.tools.foundation.core.ecf.messages"; //$NON-NLS-1$

	public static String ECFExamplesTransport_Downloading;
	public static String ECFExamplesTransport_Internal_Error;
	public static String ECFExamplesTransport_IO_error;
	public static String ECFExamplesTransport_Loading;
	public static String ECFExamplesTransport_ReceivedSize_Of_FileSize_At_RatePerSecond; 
	public static String ECFExamplesTransport_ReceivedSize_At_RatePerSecond;
	public static String ECFExamplesTransport_Server_redirected_too_many_times;
	public static String ECFTransport_Operation_canceled;
	public static String ECFExamplesTransport_Initialization_error;
	public static String ECFExamplesTransport_File_Timestamp_Error;
	
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(RESOURCE, Messages.class);
	}

	private Messages() {
	}
}
