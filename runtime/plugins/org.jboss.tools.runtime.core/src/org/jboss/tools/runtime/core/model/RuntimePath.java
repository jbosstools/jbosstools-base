/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.runtime.core.model.ServerDefinition;

/**
 * @author snjeza
 *
 */
public class RuntimePath implements Cloneable {

	private String path;
	private boolean scanOnEveryStartup;
	private List<ServerDefinition> serverDefinitions;
	private long timestamp;

	public RuntimePath(String path) {
		this.path = path;
		this.scanOnEveryStartup = false;
		this.serverDefinitions = new ArrayList<ServerDefinition>();
		this.timestamp = -1;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public boolean isScanOnEveryStartup() {
		return scanOnEveryStartup;
	}

	public void setScanOnEveryStartup(boolean scanOnEveryStartup) {
		this.scanOnEveryStartup = scanOnEveryStartup;
	}

	public List<ServerDefinition> getServerDefinitions() {
		return serverDefinitions;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		RuntimePath runtimePath = new RuntimePath(path);
		runtimePath.setScanOnEveryStartup(scanOnEveryStartup);
		runtimePath.serverDefinitions = (List<ServerDefinition>) ((ArrayList<ServerDefinition>)serverDefinitions).clone();
		return runtimePath;
	}

	public boolean isModified() {
		if (path == null || path.isEmpty()) {
			return false;
		}
		if (timestamp < 0) {
			return true; 
		}
		
		File directory = new File(path); 
		if (! directory.isDirectory()){
			return false;
		}
		try {
			return directory.lastModified() > timestamp;
		} catch (Exception e) {
			return false;
		}
	}
	
	@Override
	public String toString() {
		return "RuntimePath [path=" + path + "]";
	}

	public long getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

}
