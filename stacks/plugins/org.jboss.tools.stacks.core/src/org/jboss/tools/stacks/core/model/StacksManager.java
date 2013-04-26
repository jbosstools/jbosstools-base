/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.stacks.core.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.jdf.stacks.client.StacksClient;
import org.jboss.jdf.stacks.client.StacksClientConfiguration;
import org.jboss.jdf.stacks.model.Stacks;
import org.jboss.jdf.stacks.parser.Parser;
import org.jboss.tools.common.core.ecf.ECFTransportUtility;
import org.jboss.tools.stacks.core.StacksCoreActivator;

public class StacksManager {

	/**
	 * TODO This property string could be changed?
	 */
	private static final String STACKS_URL_PROPERTY = "org.jboss.examples.stacks.url";
	private static final String STACKS_URL;

	static {
		String defaultUrl = getStacksUrlFromJar(); //$NON-NLS-1$
		STACKS_URL = System.getProperty(
				STACKS_URL_PROPERTY, defaultUrl); //$NON-NLS-1$
	}

	public Stacks getStacks(IProgressMonitor monitor) {
		Stacks stacks = null;
		try {
			File f = ECFTransportUtility.getFileFromURL(new URL(STACKS_URL),
					"stacks", "yaml", monitor);//$NON-NLS-1$ //$NON-NLS-2$
			if (f != null && f.exists()) {
				FileInputStream fis = null;
				try {
					fis = new FileInputStream(f);
					Parser p = new Parser();
					stacks = p.parse(fis);
				} finally {
					close(fis);
				}
			}
		} catch (Exception e) {
			StacksCoreActivator.log(e, "Can't access or parse  " + STACKS_URL ); //$NON-NLS-1$
		}
		if (stacks == null) {
			StacksCoreActivator.log("Stacks from "+STACKS_URL +" can not be read, falling back on default Stacks Client values");
			StacksClient client = new StacksClient();
			stacks = client.getStacks();
		}
		return stacks;

	}

	private static String getStacksUrlFromJar() {
		InputStream is = null;
		try {
			is = StacksManager.class.getResourceAsStream("/org/jboss/jdf/stacks/client/config.properties"); //$NON-NLS-1$
			Properties p = new Properties();
			p.load(is);
			return p.getProperty(StacksClientConfiguration.REPO_PROPERTY);
		} catch (Exception e) {
			System.err.println("Can't read stacks url from the stacks-client.jar"); //$NON-NLS-1$
			e.printStackTrace();
		} finally {
			close(is);
		}
		return null;
	}
	
	private static void close(InputStream is) {
		if( is != null ) {
			try {
				is.close();
			} catch(IOException ie) {
				// IGNORE
			}
		}
	}
	
}
