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
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author snjeza
 *
 */
public interface IRuntimeDetector extends Comparable<IRuntimeDetector> {
	/**
	 * The framework is asking you to create or otherwise initialize 
	 * the runtimes provided in this list. 
	 * 
	 * @param runtimeDefinitions
	 */
	void initializeRuntimes(List<RuntimeDefinition> runtimeDefinitions);

	/**
	 * The framework is asking this detector to search the given folder
	 * and return a runtime definition, or null if this folder
	 * is not a recognized runtime
	 * 
	 * @param root
	 * @param monitor
	 * @return
	 */
	RuntimeDefinition getRuntimeDefinition(File root, IProgressMonitor monitor);
	
	/**
	 * The framework is asking you to check nested folders for 
	 * additional runtimes that may be provided.
	 * 
	 * @param runtimeDefinition
	 */
	void computeIncludedRuntimeDefinition(RuntimeDefinition runtimeDefinition);

	void setName(String name);

	void setPreferenceId(String preferenceId);
	
	void setId(String id);

	String getName();

	String getPreferenceId();
	
	String getId();
	
	void setEnabled(boolean enabled);
	
	boolean isEnabled();
	

	boolean exists(RuntimeDefinition serverDefinition);
	
	int getPriority();
	
	void setPriority(int priority);
	
	
	boolean isValid();
	
	void setValid(boolean valid);
	
	String getVersion(RuntimeDefinition runtimeDefinition);
}
