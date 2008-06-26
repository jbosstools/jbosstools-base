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
package org.jboss.tools.common.model.ui.templates.model;

public class MetaValue {
	MetaValue parent;
	String value = null;
	
	public void setParent(MetaValue parent) {
		this.parent = parent;
	}
	
	public MetaValue getParent() {
		return parent;
	}
	
	public String getValue() {
		return (value != null || parent == null) ? value : parent.getValue();
	}
	
	public void setValue(String value) {
		if(value == null) {
			this.value = null;
		} else {
			if(parent != null && value.equals(parent.getValue())) {
				this.value = null;
			} else {
				this.value = value;
			}
		}		
	}

	public boolean isOverriding() {
		return value != null;
	}

	public void commitToParent() {
		if(parent == null) return;
		if(value != null) {
			parent.setValue(value);
			value = null;
		}
	}

	public void loadFromParent(int depth) {
		if(depth < 1 || parent == null) return;
		if(depth == 1) {
			value = null;
		} else if(depth == 2) {
			MetaValue gp = parent.parent;
			if(gp == null) return;
			setValue(gp.value);
		}
	}

}
