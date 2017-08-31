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
package org.jboss.tools.common.model.impl;

import java.text.MessageFormat;
import java.util.*;

import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class RegularObjectImpl extends XModelObjectImpl implements XOrderedObject {
    private static final long serialVersionUID = 7942041044569562286L;
    protected RegularChildren children = null;

    public RegularObjectImpl() {}

    protected RegularChildren createChildren() {
        return new RegularChildren();
    }

    public void destroy() {
    	if(children != null && !children.isEmpty()) {
    		XModelObject[] h = children.getObjects();
    		for (XModelObject o: h) {
    			((XModelObjectImpl)o).destroy();
    		}
    		//TODO provide safe work at setting children to null.
    		children = createChildren();
    	}
    	super.destroy();
    }

    public boolean areChildrenOrdered() {
    	return children != null && children.areChildrenOrdered();
    }

    public String name() {
        return get(XModelObjectConstants.XML_ATTR_NAME);
    }

    protected void onSetEntity(String name) {
        super.onSetEntity(name);
        if(children == null) {
        	children = createChildren();
        }
        children.clear();
        children.setComparator(createComparator());
    }

    protected Comparator<XModelObject> createComparator() {
        return EntityComparator.getComparator(getModelEntity());
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    protected void loadChildren() {}

    public XModelObject[] getChildren() {
        loadChildren();
        return children.getObjects();
    }
    
    public XModelObject[] getLoadedChildren() {
    	return children.getObjects();
    }

    public boolean addChild_0(XModelObject o) {
    	if(o == null) {
    		return false;
    	}
    	String entity = o.getModelEntity().getName();
    	XChild c = getModelEntity().getChild(entity);
        if(c == null) return false;
        if(c.getMaxCount() < Integer.MAX_VALUE && children.getChildrenCount(entity) >= c.getMaxCount()) {
        	return false;
        }
        ((XModelObjectImpl)o).setParent_0(this);
        boolean b = children.addObject(o);
        if(b) {
        	if(o.getErrorState() == 2 || o.getErrorChildCount() > 0) {
        		registerErrorChild();
        	}
        	if(o.getErrorState() == 1 || o.getWarningChildCount() > 0) {
        		registerWarningChild();
        	}
        }
        return b;
    }

    public void removeChild_0(XModelObject o) {
        if(children.removeObject(o)) {
        	if(o.getErrorState() == 2 || o.getErrorChildCount() > 0) {
        		unregisterErrorChild();
        	}
        	if(o.getErrorState() == 1 || o.getWarningChildCount() > 0) {
        		unregisterWarningChild();
        	}
        }
    }

    public void set(String name, String value) {
        if(!(getParent() instanceof RegularObjectImpl)) {
        	super.set(name, value);
        	return;
        }
        String ov = super.get(name);
        String opp = getPathPart();
        super.set(name, value);
        String npp = getPathPart();
        if(opp != null && opp.equals(npp)) {
        	return;
        }
        RegularObjectImpl p = (RegularObjectImpl)getParent();
        XModelObject c = p.children.change(this, opp, npp);
        if(c != null) {
        	if(getModelEntity().getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) {
        		int k = 1;
        		while(c != null) {
        			super.set(XModelObjectLoaderUtil.ATTR_ID_NAME, Integer.toString(k)); //$NON-NLS-1$
        			npp = getPathPart();
        			c = p.children.change(this, opp, npp);
        			k++;
        		}
        		return;
        	} else if(hasIdAttr()) {
        		int k = 1;
        		while(c != null) {
        			super.set(XModelObjectImpl.DUPLICATE, Integer.toString(k)); //$NON-NLS-1$
        			npp = getPathPart();
        			if(k == 1 && npp.indexOf(XModelObjectImpl.DUPLICATE) < 0) {
        				elementExists(c, name, value);
        				return;
        			}
        			c = p.children.change(this, opp, npp);
        			k++;
        		}
        	} else {
        		if(ov == null) properties.remove(name); else super.set(name, ov);
        		elementExists(c, name, value);
        	}
        }
    }

    public XModelObject getChildByPathPart(String pathpart) {
        loadChildren();
        return children.getObject(pathpart);
    }

    // diagnostic

    /**
     * This method may be called in two cases:
     * 1) User input is not appropriately validated. In this case proper validation 
     *    and notification should be implemented (e.g. error message in dialogs, 
     *    error markers in editors/error view)
     * 2) Internal model update is implemented with errors.
     * @param o
     * @param name
     * @param value
     */
    protected void elementExists(XModelObject o, String name, String value) {
        if(getModel().getService() == null) return;
        String mes = MessageFormat.format(
				"Cannot set {0} = {1} for {2}\nbecause {3} exists in the {4}",
				getAttrNameByXMLName(name), value, title(this), title(o), title(getParent()));
        if(Display.getCurrent() != null) {
        	getModel().getService().showDialog("Error", mes, new String[]{"OK"}, null, 1);
        }
       	ModelPlugin.getDefault().logError(mes);
    }

    private static String title(XModelObject o) {
        return o.getAttributeValue(XModelObjectConstants.ATTR_ELEMENT_TYPE) + " " + //$NON-NLS-1$
               o.getModelEntity().getRenderer().getTitle(o);
    }

    private String getAttrNameByXMLName(String name) {
        if(getModelEntity().getAttribute(name) != null) return name;
        XAttribute[] as = getModelEntity().getAttributes();
        for (int i = 0; i < as.length; i++)
          if(name.equals(as[i].getXMLName())) return as[i].getName();
        return name;
    }

    // implementation of interface XOrderedObject for ordered subclasses

    public int getIndexOfChild(XModelObject o) {
        return children.getIndex(o);
    }

    public boolean move(int from, int to, boolean firechange) {
        boolean b = children.move(from, to);
        if(b) {
            fireStructureChanged(XModelTreeEvent.STRUCTURE_CHANGED, this);
            if(firechange) setModified(true);
        }
        return b;
    }
    
    //TODO implement registering/unregistering error children 
    public void replaceChildren(XModelObject[] objects) {
    	for (int i = 0; i < objects.length; i++) {
    		((XModelObjectImpl)objects[i]).setParent_0(this);
    	}
    	children.replaceChildren(objects);
    	changeTimeStamp();
    }

}

