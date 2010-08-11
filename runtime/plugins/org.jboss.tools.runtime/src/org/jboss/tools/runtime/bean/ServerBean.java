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

package org.jboss.tools.runtime.bean;

import java.io.File;

public class ServerBean {
	
	public static final String EMPTY = "";
	
	public ServerBean() {
		
	}
	
	public ServerBean(String location, String name, ServerType type,
			String version) {
		super();
		this.location = location;
		this.name = name;
		this.type = type;
		this.version = version;
	}
	
	public ServerBean(ServerBean bean) {
		this(bean.getLocation(),bean.getName(), bean.getType(),bean.getVersion());
	}

	private String location=EMPTY;
	private ServerType type = ServerType.UNKNOWN;
	private String name = EMPTY;
	private String version = EMPTY;
	
	public String getLocation() {
		return location;
	}
	
	public void setLocation(String location) {
		this.location = location;
	}
	
	public ServerType getType() {
		return type;
	}
	
	public void setType(ServerType type) {
		this.type = type;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getVersion() {
		return version;
	}
	
	public void setVersion(String version) {
		this.version = version;
	}
	
	public String toString() {
		return name + "," + type + "," + version + "," + location;
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(this == obj) return true;
		return this.toString().equals(obj.toString());
	}
}