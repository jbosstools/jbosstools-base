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

import java.util.*;

public class MetaGroup {
    private String uri;
	private HashMap<String,MetaClassTemplate> templates = new HashMap<String,MetaClassTemplate>();
	MetaGroup parent;

	public void setParent(MetaGroup parent) {
		this.parent = parent;
		if(parent != null) {
			uri = parent.getUri();
			Iterator<MetaClassTemplate> ts = parent.getTemplates().iterator();
			while(ts.hasNext()) {
				MetaClassTemplate pc = (MetaClassTemplate)ts.next();
				MetaClassTemplate t = MetaElementFactory.instance.createClassTemplate(pc);
				templates.put(t.getAxis(), t);
			}			
		}
	}
	
	public String getUri() {
		return uri;
	}
    public void setUri(String uri) {
        this.uri = uri;
    }

	public Collection<MetaClassTemplate> getTemplates() {
	    return templates.values();
	}
	
	public MetaClassTemplate getMetaTemplate(String axis) {
		return (MetaClassTemplate)templates.get(axis);
	}
	
	public MetaClassTemplate addMetaClassTemplate(String axis) {
		MetaClassTemplate t = getMetaTemplate(axis);
		if(t != null) return t;
		t = MetaElementFactory.instance.createClassTemplate(null);
		t.setAxis(axis);
		templates.put(axis, t);
		return t;		
	}

	public boolean isOverriding() {
		Iterator it = templates.values().iterator();
		while(it.hasNext()) {
			MetaClassTemplate t = (MetaClassTemplate)it.next();
			if(t.isOverriding()) return true;
		}
		return false;
	}

	public void commitToParent() {
		if(parent == null) return;
		Iterator ts = getTemplates().iterator();
		while(ts.hasNext()) {
			MetaClassTemplate pc = (MetaClassTemplate)ts.next();
			pc.commitToParent();
		}			
	}

	public void loadFromParent(int depth) {
		if(parent == null) return;
		Iterator ts = getTemplates().iterator();
		while(ts.hasNext()) {
			MetaClassTemplate pc = (MetaClassTemplate)ts.next();
			pc.loadFromParent(depth);
		}			
	}

}
