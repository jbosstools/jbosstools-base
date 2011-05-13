/*************************************************************************************
 * Copyright (c) 2011 JBoss by Red Hat and others.
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
 * 
 * @author snjeza
 *
 */
public class InvalidRuntimeDetector extends AbstractRuntimeDetector {

	@Override
	public void initializeRuntimes(List<ServerDefinition> serverDefinitions) {
		
	}

	@Override
	public ServerDefinition getServerDefinition(File root,
			IProgressMonitor monitor) {
		return null;
	}

	@Override
	public boolean exists(ServerDefinition serverDefinition) {
		return false;
	}

}
