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
package org.jboss.tools.common.meta.ui.editor;

import java.util.*;
import org.w3c.dom.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.util.*;

public class MetaLoaderUtil extends XModelObjectLoaderUtil {

    public MetaLoaderUtil() {
		Hashtable _singular = new Hashtable();
		_singular.put("kind", "kind"); //$NON-NLS-1$ //$NON-NLS-2$
		setup(_singular, false);
    }
    
    public void load(Element element, XModelObject o) {
        loadAttributes(element, o);
        if(isLoadOrderedChildren(o)) {
            loadChildrenOrdered(element, o);
        } else {
            loadChildren(element, o);
        }
    }

    private static boolean isLoadOrderedChildren(XModelObject o) {
        if(!(o instanceof MetaElementOrderedImpl)) return false;
        if(o.getModelEntity().getChildren().length < 2) return false;
        return true;
    }

    public void loadChildren(Element element, XModelObject o) {
        XModelEntity entity = o.getModelEntity();
        XChild[] childs = entity.getChildren();
        for (int i = 0; i < childs.length; i++) if(childs[i].isRequired()) {
          try {
              o.addChild(o.getModel().createModelObject(childs[i].getName(), new Properties()));
          } catch (Exception e) {}
        }
        for (int i = 0; i < childs.length; i++) {
            String en = childs[i].getName();
            XModelEntity ce = entity.getMetaModel().getEntity(en);
            Element[] es = XMLUtil.getAncestors(element, ce.getXMLSubPath());
            for (int j = 0; j < es.length; j++) {
                if(!en.equals(calculateEntity(es[j], en))) continue;
                XModelObject c = o.getModel().createModelObject(en, new Properties());
                boolean add = true;
                if(childs[i].isRequired()) {
                    XModelObject q = o.getChildByPath(c.getPathPart());
                    if(q != null) {
                        c = q;
                        add = false;
                    }
                }
                load(es[j], c);
                if(add) o.addChild(c);
            }
        }
    }

    public void loadChildrenOrdered(Element element, XModelObject o) {
        XModelEntity entity = o.getModelEntity();
        XChild[] childs = entity.getChildren();
        XModelEntity[] es = new XModelEntity[childs.length];
        for (int i = 0; i < es.length; i++)
          es[i] = entity.getMetaModel().getEntity(childs[i].getName());
        NodeList nl = element.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            if(n.getNodeType() != Node.ELEMENT_NODE) continue;
            Element ce = (Element)n;
            String en = calculateEntity(ce, es);
            XModelObject c = o.getModel().createModelObject(en, new Properties());
            load(ce, c);
            o.addChild(c);
        }
    }
    
    private static String calculateEntity(Element element, XModelEntity[] es) {
    	String entity = getModelEntityAttribute(element);
    	if(entity != null) return entity;
        String xml = element.getNodeName();
        for (int i = 0; i < es.length; i++) {
            if(xml.equals(es[i].getXMLSubPath()))
              return calculateEntity(element, es[i].getName());
        }
        return null;
    }

    private static String calculateEntity(Element element, String def) {
    	String entity = getModelEntityAttribute(element);
    	if(entity != null) return entity;
        if(def.equals("MetaActionList")) { //$NON-NLS-1$
            String k = element.getAttribute("kind"); //$NON-NLS-1$
            if("action".equals(k)) return "MetaAction"; //$NON-NLS-1$ //$NON-NLS-2$
        }
        return def;
    }

    public boolean save(Element parent, XModelObject o) {
    	if(!needToSave(o)) return true;
    	return super.save(parent, o);
    }
    boolean needToSave(XModelObject o) {
    	if(o == null) return false;
    	String entity = o.getModelEntity().getName();
    	if("MetaAttributeConstraint".equals(entity) //$NON-NLS-1$
    		|| "MetaAttributeEditor".equals(entity)) { //$NON-NLS-1$
    		return (hasSetAttributes(o) 
    				|| o.getChildren().length > 0
    				);
    	}
    	return true;
    }
    
    private boolean hasSetAttributes(XModelObject o) {
    	XAttribute[] as = o.getModelEntity().getAttributes();
    	for (int i = 0; i < as.length; i++) {
    		String xml = as[i].getXMLName();
    		// it would be more safe to check isSavable
    		if(xml == null || xml.length() == 0 || "NAME".equals(xml)) continue; //$NON-NLS-1$
    		String v = o.getAttributeValue(as[i].getName());
    		if(v != null && v.length() > 0 && !v.equals(as[i].getDefaultValue())) return true;
    	}
    	String finalComment = o.get("#final-comment"); //$NON-NLS-1$
    	if(finalComment != null && finalComment.length() > 0) return true;
    	return false;
    }

}
