/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug;

public interface IPropertyKeys {
	/** The preference key for VM Install containing a jdk */
	static final String JDK_VM_INSTALL = RemoteDebugActivator.PLUGIN_ID + ".jdkVMInstall";
	
	/**
	 * A key to be set on launch configs to mark them as default
	 */
	public static final String SET_AS_DEFAULT = "setAsDefault"; //$NON-NLS-1$
	

}
