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
package org.jboss.tools.common.model.util;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.jboss.tools.common.model.plugin.ModelPlugin;

public class SafeDocumentBuilderFactory extends Thread {
	DocumentBuilder d = null;
	boolean validate = false;
	
	private SafeDocumentBuilderFactory(boolean validate) throws Exception {
		this.validate = validate;
		setContextClassLoader(getClass().getClassLoader());
		start();
		join();
	}
	
	public void run () {
		try {
			DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
			if(validate) f.setValidating(validate);
			d = f.newDocumentBuilder();				
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
	}
	
	public static DocumentBuilder createDocumentBuilder(boolean validating) {
/*
 * This Thread call is workaround that except empty thread Context ClassLoader
 */
		try {
			return new SafeDocumentBuilderFactory(validating).d;
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
			return null;
		} 
	}
	
}