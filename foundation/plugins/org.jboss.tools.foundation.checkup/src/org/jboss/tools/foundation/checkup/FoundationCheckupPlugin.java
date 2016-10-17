/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.checkup;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;
import org.osgi.framework.BundleContext;

public class FoundationCheckupPlugin extends AbstractUIPlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.foundation.checkup"; //$NON-NLS-1$
	private static FoundationCheckupPlugin instance;
	private static BundleContext myContext;
	
	private JVMProblemModel model = null;
	
	public FoundationCheckupPlugin() {
		super();
		instance = this;
	}

	public static FoundationCheckupPlugin getDefault() {
	    return instance;
	}

	public static BundleContext getBundleContext() {
	    return myContext;
	}

    public void start(BundleContext context) throws Exception {
        super.start(context);
        myContext = context;
        model = JVMProblemModel.getInstance();
	}
    
    public void stop(BundleContext context) throws Exception {
    	model.cancelDetectorJob();
    	myContext = null;
    	super.stop(context);
    }
    
	/**
	 * Gets message from plugin.properties
	 * @param key
	 * @return
	 */
	public static String getMessage(String key)	{
		return Platform.getResourceString(instance.getBundle(), key);
	}
	
	public static void logError(Throwable t){
		String pluginId = instance.getBundle().getSymbolicName();
		String message = null;
		if (t != null) {
			message = t.getMessage();
		}
		Status status = new Status(IStatus.WARNING, pluginId, 0, message, t);
		instance.getLog().log(status);
	}

}
