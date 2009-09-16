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
package org.jboss.tools.common.meta.help;

import java.io.*;
import java.text.MessageFormat;
import java.util.*;
import java.net.URL;
import java.util.zip.*;
import org.eclipse.core.runtime.*;
import org.eclipse.ui.help.WorkbenchHelp;
import org.osgi.framework.Bundle;

import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.impl.handlers.OpenWithExternalBrowserHandler;
import org.jboss.tools.common.meta.action.impl.handlers.OpenWithExternalHandler;
import org.jboss.tools.common.meta.key.KeyLoader;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class HelpUtil {
	private static final String HELP_PLUGIN_ID = "org.jboss.tools.struts.doc.ui"; //$NON-NLS-1$
	private static final String JSF_HELP_PLUGIN_ID = "org.jboss.tools.jsf.doc.ui"; //$NON-NLS-1$

	static String[] PLUGINS = new String[]{HELP_PLUGIN_ID, JSF_HELP_PLUGIN_ID};
    static Properties keys = KeyLoader.load("help/keys"); //$NON-NLS-1$

    public static boolean hasHelp(String key) {
        String path = (key == null) ? null : keys.getProperty(key);
        return path != null && path.length() > 0;
    }

    public static boolean isHelpPluginInstalled() {
    	for (int i = 0; i < PLUGINS.length; i++)
    	  if(Platform.getBundle(PLUGINS[i]) != null) return true;
    	return false;
    }

	public static boolean hasEclipseHelp(String key) {
		String path = (key == null) ? null : keys.getProperty(key);
		return path != null && path.length() > 0;
	}

    public static void callExternalBrowser(XModel model, String url) throws XModelException {
        XModelObject editor = model.getByPath("%Options%/External Programs/Internet Browser"); //$NON-NLS-1$
        if(editor == null) throw new XModelException("External Program 'Internet Browser' is not set in Options.");
        OpenWithExternalBrowserHandler.start("Help", url, editor); //$NON-NLS-1$
    }

    public static String createKey(XModelObject object, XAction action) {
        return object.getModelEntity().getName() + "_" + action.getName(); //$NON-NLS-1$
    }

    public static Properties createKey(XModelObject object, XAction action, Properties p) {
        if(p == null) p = new Properties();
        p.setProperty("help", createKey(object, action)); //$NON-NLS-1$
        return p;
    }

	public static void helpEclipse(XModel model, String key) {
		ServiceDialog d = model.getService();
		if(!isHelpPluginInstalled()) {
			d.showDialog("Help", "User Guide is not installed.", 
					new String[]{"Close"}, null, ServiceDialog.MESSAGE);
		} else if(!hasEclipseHelp(key)) {
			d.showDialog("Help", MessageFormat.format("Help key {0} is not found.", key), 
					new String[]{"Close"}, null, ServiceDialog.MESSAGE);
		} else {
			String path = getValidPath(keys.getProperty(key));
			if (path != null) {
				WorkbenchHelp.displayHelpResource(path);
			} else {
				d.showDialog("Help", MessageFormat.format("Help resource {0} is not found.",
						keys.getProperty(key)), new String[]{"Close"}, null, ServiceDialog.MESSAGE);
			}
		}
	}
	
	private static String getValidPath(String path) {
		if(path == null) return null;
		for (int i = 0; i < PLUGINS.length; i++) {
			Bundle p = Platform.getBundle(PLUGINS[i]);
			URL url = EclipseResourceUtil.getInstallURL(p);
			if(url == null) continue;
			String f = url.getFile().replace('\\', '/');
			if(f.endsWith(XModelObjectConstants.SEPARATOR)) f = f.substring(0, f.length() - 1);
			if(!path.startsWith(XModelObjectConstants.SEPARATOR)) path = XModelObjectConstants.SEPARATOR + path;
			String zipPath = f + "/doc.zip"; //$NON-NLS-1$
			if(new File(zipPath).exists()) {
				 Set set = getZipEntries(zipPath);
				 if(set.contains(path)) return XModelObjectConstants.SEPARATOR + PLUGINS[i] + XModelObjectConstants.SEPARATOR + path;
			}
			if(new java.io.File(f + path).isFile()) return XModelObjectConstants.SEPARATOR + PLUGINS[i] + XModelObjectConstants.SEPARATOR + path;			
		}
		return null;
	}
	
	static Map<String,Set<String>> zips = new HashMap<String,Set<String>>();
	
	private static Set<String> getZipEntries(String zipPath) {
		Set<String> set = zips.get(zipPath);
		if(set != null) return set;
		set = new HashSet<String>();
		zips.put(zipPath, set);
		try {
			ZipFile zip = new ZipFile(new File(zipPath));
			Enumeration en = zip.entries();
			while(en.hasMoreElements()) {
				ZipEntry entry = (ZipEntry)en.nextElement();
				set.add(XModelObjectConstants.SEPARATOR + entry.getName());
			}
		} catch (IOException e) {
			//ignore
		}
		return set;
	}

}
