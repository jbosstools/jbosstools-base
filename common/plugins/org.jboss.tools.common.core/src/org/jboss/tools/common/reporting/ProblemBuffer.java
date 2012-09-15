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

import java.io.File;

import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.util.FileUtils;
import org.osgi.framework.Bundle;

/**
 * This class is intended only to be called by Report Problem Framework.
 * @author glory
 */
public class ProblemBuffer {

	ProblemBuffer() {}

	/**
	 * Returns number of entries in .log file.
	 * @return number of entries in .log file
	 */	
	public int getSize() {
		String s = getContent();
		if(s.length() == 0) {
			return 0;
		}
		int i = 0;
		int c = 0;
		while(i < s.length()) {
			i = s.indexOf("!ENTRY", i); //$NON-NLS-1$
			if(i >= 0) {
				++c;
				++i;
			} else {
				break;
			}
		}		
		return c;
	}
	
	/**
	 * Returns content of .log file.
	 * @return content of .log file
	 */	
	public String getContent() {
		File f = getLogFile();
		return (!f.isFile()) ? "" : FileUtils.readFile(f); //$NON-NLS-1$
	}
	
	public String getEclipseLogContent() {
		File f = Platform.getLogFileLocation().toFile();
		return (f.isFile()) ? FileUtils.readFile(f) : ""; //$NON-NLS-1$
	}
	
	public void clean() {
		File f = getLogFile();
		if(f.exists()) {
			f.delete();
		}
	}
	
	/**
	 * @param text
	 * @param userEMail
	 * @param other
	 */
	public void report(String text, String userEMail, String other, boolean cleanBuffer) {
		StringBuffer sb = new StringBuffer();
		if(userEMail != null && userEMail.trim().length() > 0) {
			sb.append("Customer e-mail=" + userEMail + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if(other != null && other.length() > 0) {
			sb.append("Other=" + other + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if(text != null) {
			sb.append(text);
		}
		String reportText = sb.toString();
		Submit.getInstance().submit(reportText, cleanBuffer);
	}

	private File getLogFile() {
		Bundle b = Platform.getBundle("org.jboss.tools.common"); //$NON-NLS-1$
		String stateLocation = Platform.getStateLocation(b).toString().replace('\\', '/');
		String logLocation = stateLocation + "/.log"; //$NON-NLS-1$
		return new File(logLocation);
	}
}