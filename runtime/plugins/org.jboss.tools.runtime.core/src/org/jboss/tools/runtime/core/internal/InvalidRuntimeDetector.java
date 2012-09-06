/*************************************************************************************
 * Copyright (c) 2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.internal;


/**
 * 
 * @author snjeza
 *
 */
public class InvalidRuntimeDetector extends RuntimeDetector {
	public InvalidRuntimeDetector(String name, String id, String preferenceId,
			int priority) {
		super(name, id, preferenceId, priority, null);
	}
	
	@Override
	public boolean isEnabled() {
		return false;
	}
	
	// Framework method
	public void setEnabled(boolean enabled) {
		// Ignore, invalid cannot be enabled
	}
}
