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
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Runtime detectors and their delegates are *NOT* expected to maintain state. 
 * They are instantiated only once and may be given several requests, 
 * possibly concurrently. 
 * 
 * @author rob stryker
 *
 */
public interface IRuntimeDetectorDelegate {
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
	 * It can be assumed that the detector is enabled when
	 * this method is called. 
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

	/**
	 * Standard impl will simply return runtimeDefinition.getVersion(), 
	 * but other impls may poll their respective models.
	 * 
	 * @param runtimeDefinition
	 * @return
	 */
	String getVersion(RuntimeDefinition runtimeDefinition);

	/**
	 * Standard impl will simply return runtimeDefinition.exists(), 
	 * but other impls may poll their respective models.
	 * 
	 * @param runtimeDefinition
	 * @return
	 */
	boolean exists(RuntimeDefinition runtimeDefinition);
	
}
