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
import java.io.IOException;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

/**
 * @author snjeza
 *
 */
public abstract class AbstractRuntimeDetector implements IRuntimeDetector {
	private String id;
	private String name;
	private String preferenceId;
	private boolean enabled;
	private boolean valid = true;
	private int priority;

	@Override
	public String getName() {
		return name;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getPreferenceId() {
		return preferenceId;
	}

	@Override
	public void setPreferenceId(String preferenceId) {
		this.preferenceId = preferenceId;
	}
	
	public String getImplementationVersion(File dir, String file) {
		File jarFile = new File(dir, file);
		if(!jarFile.isFile()) {
			return null;
		}
		try {
			JarFile jar = new JarFile(jarFile);
			Attributes attributes = jar.getManifest().getMainAttributes();
			String version = attributes.getValue("Implementation-Version");
			return version;
		} catch (IOException e) {
			return null;
		}
	}

	public boolean isEnabled() {
		if (!isValid()) {
			return false;
		}
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
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
		AbstractRuntimeDetector other = (AbstractRuntimeDetector) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}

	public int getPriority() {
		return priority;
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	@Override
	public int compareTo(IRuntimeDetector o) {
		if (o == null) {
			return 1;
		}
		int p1 = this.getPriority();
		int p2 = o.getPriority();
		return p1 - p2;
	}

	@Override
	public void computeIncludedServerDefinition(
			ServerDefinition serverDefinition) {
		
	}
	
	public boolean isValid() {
		return valid;
	}

	public void setValid(boolean valid) {
		this.valid = valid;
	}
}
