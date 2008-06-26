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

public class MetaClassTemplate {
	private MetaClassTemplate parent;
	private String axis;
	private String displayName;
	private String xEntity;
	private MetaValue superClass;
	private MetaValueList interfaces;
	
	public void setParent(MetaClassTemplate parent) {
		this.parent = parent;
		if(parent == null) {
			superClass = MetaElementFactory.instance.createValue(null);
			interfaces = MetaElementFactory.instance.createValueList(null);
		} else {
			axis = parent.getAxis();
			displayName = parent.getDisplayName();
			xEntity = parent.getXEntity();
			superClass = MetaElementFactory.instance.createValue(parent.getSuperClass());
			interfaces = MetaElementFactory.instance.createValueList(parent.getInterfaces());
		}
	}
	
	public MetaValue getSuperClass() {
		return superClass;
	}
	
	public MetaValueList getInterfaces() {
		return interfaces;
	}
	
	public String getAxis() {
		return axis;
	}

	public String getDisplayName() {
		return displayName;
	}

	public String getXEntity() {
		return xEntity;
	}

	public void setAxis(String axis) {
    	this.axis = axis;
    }

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public void setXEntity(String entity) {
		xEntity = entity;
	}
    
	public boolean isOverriding() {
		return (superClass.isOverriding() && !isOverridingDefaultClass()) || interfaces.isOverriding();
	}
	
	private boolean isOverridingDefaultClass() {
		if(!"java.lang.Object".equals(superClass.getValue())) return false;
		String pv = (parent == null) ? null : parent.getSuperClass().getValue();
		return pv == null || pv.length() == 0;
	}

	public void commitToParent() {
		if(parent == null) return;
		superClass.commitToParent();
		interfaces.commitToParent();
	}

	public void loadFromParent(int depth) {
		if(parent == null) return;
		superClass.loadFromParent(depth);
		interfaces.loadFromParent(depth);
	}
}
