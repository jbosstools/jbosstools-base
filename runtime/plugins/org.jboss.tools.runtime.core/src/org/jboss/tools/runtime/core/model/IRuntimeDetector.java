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


/**
 * @author snjeza
 *
 */
public interface IRuntimeDetector extends  IRuntimeDetectorDelegate, Comparable<IRuntimeDetector> {

	String getName();

	String getPreferenceId();
	
	String getId();
	
	boolean isEnabled();
	
	void setEnabled(boolean enabled);
	
	int getPriority();
	
	boolean isValid();
}
