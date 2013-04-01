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

/**
 * A runtime definition represents a specific runtime type and 
 * version, at some location, and what name it has.
 * 
 * @author snjeza
 *
 */
public class RuntimeDefinition {

	private String name;
	private String version;
	private String type;
	private File location;
	private String description;
	private boolean enabled = true;
	private RuntimePath runtimePath;
	private List<RuntimeDefinition> includedRuntimeDefinitions = new ArrayList<RuntimeDefinition>();
	private RuntimeDefinition parent;
	
	public RuntimeDefinition(String name, String version, 
			String type, File location) {
		super();
		this.name = name;
		this.version = version;
		this.type = type;
		this.location = location;
		this.description = ""; //$NON-NLS-1$
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
	
	public String getType() {
		return type;
	}
	
	public void setType(String type) {
		this.type = type;
	}
	
	public File getLocation() {
		return location;
	}
	
	public void setLocation(File location) {
		this.location = location;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((location == null) ? 0 : location.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		result = prime * result + ((version == null) ? 0 : version.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RuntimeDefinition other = (RuntimeDefinition) obj;
		if (location == null) {
			if (other.location != null)
				return false;
		} else if (!location.equals(other.location))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		if (version == null) {
			if (other.version != null)
				return false;
		} else if (!version.equals(other.version))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "RuntimeDefinition [name=" + name + ", version=" + version //$NON-NLS-1$ //$NON-NLS-2$
				+ ", type=" + type + ", location=" + location + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public RuntimePath getRuntimePath() {
		return runtimePath;
	}

	public void setRuntimePath(RuntimePath runtimePath) {
		this.runtimePath = runtimePath;
	}

	public List<RuntimeDefinition> getIncludedRuntimeDefinitions() {
		return includedRuntimeDefinitions;
	}

	public RuntimeDefinition getParent() {
		return parent;
	}

	public void setParent(RuntimeDefinition parent) {
		this.parent = parent;
	}
	
}
