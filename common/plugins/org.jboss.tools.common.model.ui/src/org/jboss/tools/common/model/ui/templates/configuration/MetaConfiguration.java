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
package org.jboss.tools.common.model.ui.templates.configuration;

import java.util.*;
import org.jboss.tools.common.model.ui.templates.model.*;

public class MetaConfiguration {
	MetaConfiguration parent;
    private HashMap<String,MetaGroup> groups = new HashMap<String,MetaGroup>();
    IMetaConfigurationSave saveAgent;
	
	public void setParent(MetaConfiguration parent) {
		this.parent = parent;
		if(parent != null) {
			Iterator gs = parent.getMetaTemplateGroups().iterator();
			while(gs.hasNext()) {
				MetaGroup p = (MetaGroup)gs.next();
				MetaGroup g = MetaElementFactory.instance.createGroup(p);
				groups.put(g.getUri(), g);
			}			
		}
	}
	
	public MetaConfiguration getParent() {
		return parent;
	}
	
	public void setSaveAgent(IMetaConfigurationSave saveAgent) {
		this.saveAgent = saveAgent;
	}
	
	public void save() {
		if(saveAgent != null) saveAgent.save();
	}

	public Collection<MetaGroup> getMetaTemplateGroups() {
		return groups.values();
    }

	public MetaGroup getMetaTemplateGroup(String publicId) {
		return (MetaGroup)groups.get(publicId);		
	}

	public MetaClassTemplate getMetaTemplate(String publicId, String axis) {
		MetaGroup g = getMetaTemplateGroup(publicId);
		return (g == null) ? null : g.getMetaTemplate(axis);
	}
	
	public MetaGroup addGroup(String publicId) {
		MetaGroup g = getMetaTemplateGroup(publicId);
		if(g != null) return g;
		g = MetaElementFactory.instance.createGroup(null);
		g.setUri(publicId);
		groups.put(publicId, g);
		return g;		
	}

	public boolean isOverriding() {
		Iterator it = groups.values().iterator();
		while(it.hasNext()) {
			MetaGroup g = (MetaGroup)it.next();
			if(g.isOverriding()) return true;
		}
		return false;
	}
	
	public void commitToParent() {
		if(parent == null) return;
		Iterator gs = getMetaTemplateGroups().iterator();
		while(gs.hasNext()) {
			MetaGroup p = (MetaGroup)gs.next();
			p.commitToParent();
		}			
	}
	
	public void loadFromParent(int depth) {
		if(parent == null) return;
		Iterator gs = getMetaTemplateGroups().iterator();
		while(gs.hasNext()) {
			MetaGroup p = (MetaGroup)gs.next();
			p.loadFromParent(depth);
		}			
	}

}
