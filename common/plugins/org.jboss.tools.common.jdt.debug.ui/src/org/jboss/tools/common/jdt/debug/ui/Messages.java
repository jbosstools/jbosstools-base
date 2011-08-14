/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui;

import org.eclipse.osgi.util.NLS;

/**
 * 
 * @author snjeza
 *
 */
public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.jdt.debug.ui.messages"; //$NON-NLS-1$
	public static String RemoteDebugItems_Configure;
	public static String Discover_Remote_Applications;
	public static String JavaConnectTab_Add_JDK;
	public static String JavaConnectTab_Host;
	public static String JavaConnectTab_Invalid_Host;
	public static String JavaConnectTab_Invalid_Port;
	public static String JavaConnectTab_JDK_Required;
	public static String JavaConnectTab_Port;
	public static String JavaConnectTab_Refresh;
	public static String JavaConnectTab_SetAsDefault;
	public static String JavaConnectTab_Warning;
	public static String JavaConnectTab__Allow_termination_of_remote_VM_6;
	public static String JavaConnectTab_Conn_ect_20;
	public static String JavaConnectTab_Connection_Properties_1;
	public static String JavaConnectTab_Project_does_not_exist_14;
	
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
