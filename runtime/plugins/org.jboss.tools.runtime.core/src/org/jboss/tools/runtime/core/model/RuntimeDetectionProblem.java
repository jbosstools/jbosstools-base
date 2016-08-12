/*************************************************************************************
 * Copyright (c) 2016 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.core.model;

import java.util.HashMap;

public class RuntimeDetectionProblem {
	private String description, label;
	private int severity, code;
	private HashMap<String, Object> properties;
	/**
	 * Create a new runtime detection problem 
	 * 
	 * @param label   		A label
	 * @param description 	A more in-depth description
	 * @param severity  	An IStatus severity key
	 * @param code			A code used to locate resolutions
	 */
	RuntimeDetectionProblem(String label, String description, int severity, int code) {
		this.label = label;
		this.description = description;
		this.code = code;
		this.severity = severity;
		properties = new HashMap<String, Object>();
	}
	public String getDescription() {
		return description;
	}
	public int getCode() {
		return code;
	}
	public String getLabel() {
		return label;
	}
	public int getSeverity() {
		return severity;
	}
	public Object getProperty(String k) {
		return properties.get(k);
	}
	public void setProperty(String k, Object v) {
		properties.put(k,  v);
	}
}
