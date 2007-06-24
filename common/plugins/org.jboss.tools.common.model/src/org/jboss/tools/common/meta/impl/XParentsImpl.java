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
package org.jboss.tools.common.meta.impl;

import java.util.*;
import org.w3c.dom.Element;
import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.meta.XParents;

/**
 * Builds ancestor-descendant dependencies of entities 
 * without loading entity details. 
 * 
 * @author glory
 */

public class XParentsImpl implements XParents, XMetaDataConstants {
	private Map<String,Set<String>> parents = new HashMap<String,Set<String>>();
	private Map<String,Set<String>> ancestors = new HashMap<String,Set<String>>();

	public boolean isDescendant(String entity1, String entity2) {
		if(!parents.containsKey(entity1)) return false;
		Set as = getAncestors(entity1);
		return as != null && as.contains(entity2);
	}

	public Set<String> getAncestors(String entity) {
		Set<String> set = ancestors.get(entity);
		if(set == null) {
			set = new HashSet<String>();
			fillAncestors(entity, set);
			ancestors.put(entity, set);
		}
		return set;
	}

	void init(XModelMetaDataImpl meta) {
		Iterator it = meta.getEntities().keySet().iterator();
		while(it.hasNext()) {
			String name = it.next().toString();
			XModelEntityImpl entity = (XModelEntityImpl)meta.getEntities().get(name);
			Element element = entity.element;
			if(element == null) {
				addParents(entity.getChildren(), name);
			} else {
				addParents(element, name);
				ArrayList<XModelEntityExtensionImpl> l = meta.getExtensions().getExtensions(name);
				if(l == null) continue;
				XModelEntityExtensionImpl[] ees = l.toArray(new XModelEntityExtensionImpl[0]);
				for (int k = 0; k < ees.length; k++) {
					Element e1 = ees[k].element;
					if(e1 == null) {
						addParents(ees[k].getChildren(), name);
					} else {
						addParents(e1, name);
					}
				}
			}
		}
    }
    
	private void addParents(XChild[] cs, String parent) {
		for (int i = 0; i < cs.length; i++) add(cs[i].getName(), parent);
	}

	private void addParents(Element element, String parent) {
		Element e = XMetaDataLoader.getUniqueChild(element, XMODEL_CHILDREN);
		if(e == null) return;
		Element[] es = XMetaDataLoader.getChildrenElements(e, XMODEL_CHILD);
		for (int i = 0; i < es.length; i++) {
			add(es[i].getAttribute(NAME), parent);
		}
	}

	private void add(String child, String parent) {
		Set<String> ps = parents.get(child);
		if(ps == null) {
			ps = new HashSet<String>();
			parents.put(child, ps);
		}
		ps.add(parent);
	}

	public Set getParents(String entity) {
		return (Set)parents.get(entity);
	}

	private void fillAncestors(String entity, Set<String> set) {
		Set parents = getParents(entity);
		if(parents == null) return;
		Iterator it = parents.iterator();
		while(it.hasNext()) {
			String e = it.next().toString();
			if(set.contains(e)) continue;
			set.add(e);
			Set<String> as = ancestors.get(entity);
			if(as != null) {
				set.addAll(as);
			} else {
				fillAncestors(e, set);
			}
		}
	}
    
}
