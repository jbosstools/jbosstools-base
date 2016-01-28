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
package org.jboss.tools.runtime.core.model;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.internal.RuntimeDetector;

public abstract class AbstractRuntimeDetectorDelegate implements
		IRuntimeDetectorDelegate {

	private boolean loggedWarning = false;
	
	/**
	 * The framework will no longer call initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions)
	 * It will instead call initializeRuntime(RuntimeDefinition runtimeDefinition)
	 */
	@Override @Deprecated
	public void initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions) {
	}

	@Override
	public boolean initializeRuntime(RuntimeDefinition runtimeDefinition) throws CoreException {
		// Provide some migration code, for now.  This code may be removed at any time in the future. 
		
		// Since this is a new method, and some clients might not be implementing it, we 
		// should redirect to the original method initializeRuntimes.
		initializeRuntimes(Collections.singletonList(runtimeDefinition));
		
		// Log an error / warning indicating other handlers should update their API call
		if( !loggedWarning ) {
			RuntimeCoreActivator.pluginLog().logWarning("Runtime Detector " + findMyDetector().getId() + " is using a deprecated API call.");
			loggedWarning = true;
		}
		
		// We don't know what to return, since we don't know if anything was created
		return false;
	}

	
	@Override
	public RuntimeDefinition getRuntimeDefinition(File root,
			IProgressMonitor monitor) {
		return null;
	}

	@Override @Deprecated
	public void computeIncludedRuntimeDefinition(
			RuntimeDefinition runtimeDefinition) {
	}

	@Override
	public String getVersion(RuntimeDefinition runtimeDefinition) {
		return runtimeDefinition.getVersion();
	}

	@Override
	public boolean exists(RuntimeDefinition runtimeDefinition) {
		return false;
	}
	
	/* These methods are only here until I can further 
	 * unravel the situation regarding nested runtimes. 
	 * They WILL BE removed asap. 
	 */
	@Deprecated
	protected boolean isEnabled() {
		IRuntimeDetector d = findMyDetector();
		return d.isEnabled();
	}
	
	@Deprecated	
	protected void setEnabled(boolean enabled) {
		IRuntimeDetector d = findMyDetector();
		((RuntimeDetector)d).setEnabled(enabled);
	}
	
	protected void loadIncludedDefinitions(RuntimeDefinition runtimeDefinition) {
		Set<IRuntimeDetector> s = getOtherRuntimeDetectors();
		for(IRuntimeDetector iNext : s) {
			if( iNext.isEnabled() ) {
				RuntimeDefinition[] result = iNext.computeIncludedDefinitions(runtimeDefinition);
				runtimeDefinition.getIncludedRuntimeDefinitions().addAll(Arrays.asList((result)));
			}
		}
	}
	
	public RuntimeDefinition[] computeIncludedDefinitions(RuntimeDefinition def) {
		// Wrap the legacy implementation. Subclasses should override
		RuntimeDefinition wrap = new RuntimeDefinition(def.getName(), def.getVersion(), def.getType(), def.getLocation(), def.getDetector());
		computeIncludedRuntimeDefinition(wrap);
		List<RuntimeDefinition> included = wrap.getIncludedRuntimeDefinitions();
		return (RuntimeDefinition[]) included.toArray(new RuntimeDefinition[included.size()]); 
	}


	protected final RuntimeDefinition createDefinition(String name, String version, String type, File loc) {
		return new RuntimeDefinition(name, version, type, loc, findMyDetector());
	}

	protected final RuntimeDefinition createDefinition(String name, String version, String type, 
			File loc, RuntimeDefinition parent) {
		RuntimeDefinition def = new RuntimeDefinition(name, version, type, loc, findMyDetector());
		def.setParent(parent);
		return def;
	}

	private Set<IRuntimeDetector> getOtherRuntimeDetectors() {
		Set<IRuntimeDetector> runtimeDetectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		TreeSet<IRuntimeDetector> cloned = new TreeSet<IRuntimeDetector>(runtimeDetectors);
		cloned.remove(findMyDetector());
		return cloned;
	}
	
	/**
	 * This method allows a runtime detector delegate to get a reference
	 * to its actual IRuntimeDetector wrapper. 
	 * @return
	 */
	protected IRuntimeDetector findMyDetector() {
		Set<IRuntimeDetector> set = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		Iterator<IRuntimeDetector> i = set.iterator();
		while(i.hasNext()) {
			IRuntimeDetector d = i.next();
			IRuntimeDetectorDelegate mightBeMe = ((RuntimeDetector)d).getDelegate();
			if( mightBeMe == this || this.equals(mightBeMe))
				return d;
		}
		return null;
	}
}
