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

public class MetaValueList {
	MetaValueList parent;
	String[] values = null;
	
	public void setParent(MetaValueList parent) {
		this.parent = parent;
	}
	
	public String[] getValues() {
		return (values == null) ? (parent == null) ? null : parent.getValues() : values;
	}
	
	public void setValues(String[] vs) {
		if(vs == null || isSameAsParent(vs)) {
			values = null;
		} else {
			values = vs;
		}
	}
	
	private boolean isSameAsParent(String[] vs) {
		if(parent == null || vs == null) return false;
		String[] pvs = parent.getValues();
		if(pvs == null || pvs.length != vs.length) return false;
		for (int i = 0; i < vs.length; i++) {
			if(!pvs[i].equals(vs[i])) return false;
		}
		return true;		
	}

	public boolean isOverriding() {
		return values != null;
	}

	public void commitToParent() {
		if(parent == null) return;
		if(values != null) {
			parent.setValues(values);
			values = null;
		}
	}

	public void loadFromParent(int depth) {
		if(depth < 1 || parent == null) return;
		if(depth == 1) {
			values = null;
		} else if(depth == 2) {
			MetaValueList gp = parent.parent;
			if(gp == null) return;
			setValues(gp.values);
		}
	}
}
