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
package org.jboss.tools.common.model.ui.texteditors;

import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.jboss.tools.common.model.XModelObject;

public class XMLModelObjectFinder {

	class Chain {
		Chain child;
		String name;
		int index;
	}
	
	public XModelObject findModelObject(IndexedRegion region, XModelObject root) {
		if(root == null) return null;
        if(!(region instanceof Node)) return null;
        Node node = (Node)region;
        while(node != null && !(node instanceof Element)) {
        	node = (Node)node.getParentNode();
        }
        if(node == null) return null;
        
        Chain c = new Chain();
        c.name = node.getNodeName();

        while(node != null) {
            Node sibling = node;
            while(sibling != null) {
            	sibling = sibling.getPreviousSibling();
            	if(sibling instanceof Element && c.name.equals(sibling.getNodeName())) c.index++;
            }
            node = (Node)node.getParentNode();
            if(!(node instanceof Element)) break;
            Chain c1 = new Chain();
            c1.name = node.getNodeName();
            c1.child = c;
            c = c1;
        }
        return findModelObject(c, root);
	}

	private XModelObject findModelObject(Chain chain, XModelObject o) {
		if(chain == null) return o;
		String n = o.getModelEntity().getXMLSubPath();
		if(n.equals(chain.name)) {
			return findModelObject(chain.child, o);
		}
		String childEntity = getChildEntity(chain.name, o);
		if(childEntity != null) {
			XModelObject[] cs = o.getChildren(childEntity);
			if(cs.length > chain.index) return findModelObject(chain.child, cs[chain.index]);
			return o;
		} else {
			XModelObject[] cs = o.getChildren();
			for (int i = 0; i < cs.length; i++) {
				if(cs[i].getModelEntity().getXMLSubPath().length() > 0) continue;
				XModelObject res = findModelObject(chain, cs[i]);
				if(res != cs[i]) return res;
			}
			return o;
		}
	}
	
	private String getChildEntity(String nodename, XModelObject o) {
		return o.getModelEntity().getChildByXML(nodename);
	}
	
}
