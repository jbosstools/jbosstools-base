/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Igels
 */
public class AttributeDescriptor {

	private String name;
	private List<AttributeValueDescriptor> valueDesriptors = new ArrayList<AttributeValueDescriptor>();
	private boolean required = false;
	private boolean preferable = false;

	/**
	 * 
	 * @return
	 */
	public boolean isPreferable() {
		return preferable;
	}

	/**
	 * 
	 * @param desired
	 */
	public void setPreferable(boolean desired) {
		this.preferable = desired;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isRequired() {
		return required;
	}

	/**
	 * 
	 * @param required
	 */
	public void setRequired(boolean required) {
		this.required = required;
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * 
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * 
	 * @return
	 */
	public AttributeValueDescriptor[] getValueDesriptors() {
		return (AttributeValueDescriptor[])valueDesriptors.toArray(new AttributeValueDescriptor[valueDesriptors.size()]);
	}

	/**
	 * 
	 * @param value
	 */
	public void addValuDescriptor(AttributeValueDescriptor value) {
		valueDesriptors.add(value);
	}
}