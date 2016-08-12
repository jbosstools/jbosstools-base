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
package org.jboss.tools.runtime.core.internal;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.model.AbstractRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimeDetectorDelegate;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;

/**
 * A class representing your runtime detector. 
 * @noinstantiate This class is not intended to be instantiated by clients
 */
public class RuntimeDetector implements IRuntimeDetector {
	
	private String name, id, preferenceId;
	private int priority;
	private IRuntimeDetectorDelegate delegate;
	private boolean enabled;
	public RuntimeDetector(String name, String id, 
			String preferenceId, int priority,
			IRuntimeDetectorDelegate delegate) {
		this.name = name;
		this.id = id;
		this.preferenceId = preferenceId;
		this.priority = priority;
		this.delegate = delegate;
		enabled = true;
	}
	
	public IRuntimeDetectorDelegate getDelegate() {
		return delegate;
	}
	
	
	@Override
	public RuntimeDefinition getRuntimeDefinition(File root,
			IProgressMonitor monitor) {
		if( delegate != null )
			return delegate.getRuntimeDefinition(root, monitor);
		return null;
	}


	@Override
	public String getVersion(RuntimeDefinition runtimeDefinition) {
		if( delegate != null )
			return delegate.getVersion(runtimeDefinition);
		return null;
	}

	@Override
	public boolean exists(RuntimeDefinition runtimeDefinition) {
		if( delegate != null)
			return delegate.exists(runtimeDefinition);
		return false;
	}

	@Override
	public int compareTo(IRuntimeDetector o) {
		if( this.equals(o))
			return 0;
		int p1 = this.getPriority();
		int p2 = o.getPriority();
		int dif = p1 - p2; 
		if( dif != 0 )
			return dif;
		return getId().compareTo(o.getId());
	}


	@Override
	public String getName() {
		return name;
	}

	@Override
	public String getPreferenceId() {
		return preferenceId;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public boolean isEnabled() {
		return enabled;
	}
	
	// Framework method
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	@Override
	public int getPriority() {
		return priority;
	}

	@Override
	public boolean isValid() {
		return true;
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
		IRuntimeDetector other = (IRuntimeDetector) obj;
		if (id == null) {
			if (other.getId() != null)
				return false;
		} else if (!id.equals(other.getId()))
			return false;
		return true;
	}

	@Override
	public boolean initializeRuntime(RuntimeDefinition runtimeDefinition) throws CoreException {
		if( delegate != null )
			return delegate.initializeRuntime(runtimeDefinition);
		return false;
	}

	@Override
	public RuntimeDefinition[] computeIncludedDefinitions(RuntimeDefinition runtimeDefinition) {
		if( delegate != null )
			return delegate.computeIncludedDefinitions(runtimeDefinition);
		return new RuntimeDefinition[0];
	}

	
	@Override @Deprecated
	public void initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions) {
		if( delegate != null )
			delegate.initializeRuntimes(runtimeDefinitions);
	}

	@Override @Deprecated
	public void computeIncludedRuntimeDefinition(
			RuntimeDefinition runtimeDefinition) {
		if( delegate != null )
			delegate.computeIncludedRuntimeDefinition(runtimeDefinition);
	}

	public void refreshProblems(RuntimeDefinition def) {
		if( delegate != null && delegate instanceof AbstractRuntimeDetectorDelegate) {
			((AbstractRuntimeDetectorDelegate)delegate).calculateProblems(def);
		}
	}
	
}
