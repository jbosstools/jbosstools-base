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
	void initializeRuntimes(List<ServerDefinition> serverDefinitions);

	void setName(String name);

	void setPreferenceId(String preferenceId);
	
	void setId(String id);

	String getName();

	String getPreferenceId();
	
	String getId();
	
	void setEnabled(boolean enabled);
	
	boolean isEnabled();
	
	ServerDefinition getServerDefinition(File root, IProgressMonitor monitor);
	
	boolean exists(ServerDefinition serverDefinition);
	
	int getPriority();
	
	void setPriority(int priority);
	
	void computeIncludedServerDefinition(ServerDefinition serverDefinition);
	
	boolean isValid();
	
	void setValid(boolean valid);
}
