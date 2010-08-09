 /*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.types;

/**
 * Driver entity for usage for DatabaseHelper
 * @author jpeterka
 *
 */
public class DriverEntity {
	String instanceName = "Hypersonic DB";
	String drvPath = "";
	String jdbcString = "";
	String profileName = "DefaultDS";
	String profileDescription = "Hypersonic embedded database";
	String databaseName = "Default";
	
	public String getInstanceName() {
		return instanceName;
	}
	public void setInstanceName(String instanceName) {
		this.instanceName = instanceName;
	}
	public String getDrvPath() {
		return drvPath;
	}
	public void setDrvPath(String drvPath) {
		this.drvPath = drvPath;
	}
	public String getJdbcString() {
		return jdbcString;
	}
	public void setJdbcString(String jdbcString) {
		this.jdbcString = jdbcString;
	}
	public String getProfileName() {
		return profileName;
	}
	public void setProfileName(String profileName) {
		this.profileName = profileName;
	}
	public String getProfileDescription() {
		return profileDescription;
	}
	public void setProfileDescription(String profileDescription) {
		this.profileDescription = profileDescription;
	}
	public String getDatabaseName() {
		return databaseName;
	}
	public void setDatabaseName(String databaseName) {
		this.databaseName = databaseName;
	}
	
	
}
