/******************************************************************************* 
* Copyright (c) 2007 Red Hat, Inc.
* Distributed under license by Red Hat, Inc. All rights reserved.
* This program is made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution,
* and is available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors:
*     Red Hat, Inc. - initial API and implementation
******************************************************************************/
package org.jboss.tools.tests;

/**
 * @author Max Areshkau
 * 
 * Contains information about projects which should be imported
 */
public class ImportBean {
	/**
	 * Contains import project Name
	 */
	private String importProjectName;
	/**
	 * Contains import project path
	 */
	private String importProjectPath;
	/**
	 * @return the importProjectName
	 */
	public String getImportProjectName() {
		return importProjectName;
	}
	/**
	 * @param importProjectName the importProjectName to set
	 */
	public void setImportProjectName(String importProjectName) {
		this.importProjectName = importProjectName;
	}
	/**
	 * @return the importProjectPath
	 */
	public String getImportProjectPath() {
		return importProjectPath;
	}
	/**
	 * @param importProjectPath the importProjectPath to set
	 */
	public void setImportProjectPath(String importProjectPath) {
		this.importProjectPath = importProjectPath;
	}

}
