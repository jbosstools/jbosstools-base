/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.common.model.options.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.osgi.framework.Bundle;

public class XStudioContribution {
	String loader;
	String resource;
	
	String[] targets = new String[0];
	int priority = 0;

	public XStudioContribution() {}

	public InputStream getInputStream() {
    	try {
            Bundle b = Platform.getBundle(loader);
        	URL u = b.getResource(resource);
        	if(u == null) {
        		ModelPlugin.getPluginLog().logWarning("Cannot find resource " + resource + " in plugin " + b); //$NON-NLS-1$ //$NON-NLS-2$
        		return null;
        	}
            URLConnection c = u.openConnection();
            return c.getInputStream();
    	} catch (IOException e) {
    		ModelPlugin.getPluginLog().logError(e);
    		return null;
    	} catch (NoClassDefFoundError e2) {
    		ModelPlugin.getPluginLog().logError(e2);
    		return null;
    	}
	}
	

}
