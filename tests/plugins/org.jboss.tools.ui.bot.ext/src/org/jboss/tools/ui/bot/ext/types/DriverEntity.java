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
	/*
	 * Defaults for Hibernate
	 */
	String instanceName = "Hypersonic DB";
	String drvPath = "";
	String jdbcString = "";
	String profileName = "DefaultDS";
	String profileDescription = "Hypersonic embedded database";
	String databaseName = "Default";
	String user = "sa";
	String pwd = "";
	String driverTemplateDescId = "org.eclipse.datatools.enablement.hsqldb.1_8.driver";
	String driverDefId = "DriverDefn.Hypersonic DB";
	
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
	public String getUser(){
		return user;
	}
	public String getPassword(){
		return pwd;
	}
	public String getDriverTemplateDescId(){
		return driverTemplateDescId;
	}
	public String getDriverDefId(){
		return driverDefId;
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
	public void setPassword(String pwd){
		this.pwd = pwd;
	}
	public void setUser(String user){
		this.user = user;
	}
	public void setDriverTemplateDescId(String id){
		this.driverTemplateDescId = id; 
	}
	public void setDriverDefId(String id){
		this.driverDefId = id;
	}
	
}
