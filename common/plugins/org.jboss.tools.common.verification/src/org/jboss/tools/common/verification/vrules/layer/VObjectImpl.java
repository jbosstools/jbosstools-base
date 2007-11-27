/*
 * VObjectImpl.java
 *
 * Created on July 14, 2003, 10:32 AM
 */

package org.jboss.tools.common.verification.vrules.layer;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.model.*;

/**
 *
 * @author  valera
 */
public class VObjectImpl implements VObject {
    
    private XModelObject modelObject;
    private VModel model;
    
    /** Creates a new instance of VObjectImpl */
    public VObjectImpl(XModelObject modelObject, VModel model) {
        this.modelObject = modelObject;
        this.model = model;
    }
    
    public Object getAttribute(String name) {
        return modelObject.getAttributeValue(name);
    }
    
    public VObject getChild(String path) {
        XModelObject child = modelObject.getChildByPath(path);
        return child == null ? null : new VObjectImpl(child, model);
    }
    
    static VObject[] EMPTY_CHILDREN = new VObject[0];
    
    public VObject[] getChildren() {
        if("true".equals(modelObject.get("overlapped"))) return EMPTY_CHILDREN;
        
        //Takes too much time, do not verify in jars
        if(modelObject.getModelEntity().getName().equals("FileSystemJar")) {
        	return EMPTY_CHILDREN;
        }
        
        XModelObject[] c = modelObject.getChildren();
        VObject[] children = new VObject[c.length];
        for (int i = 0; i < c.length; i++) {
            children[i] = new VObjectImpl(c[i], model);
        }
        return children;
    }
    
    public VEntity getEntity() {
        return model.getEntity(modelObject.getModelEntity().getName());
    }
    
    public VModel getModel() {
        return model;
    }
    
    public VObject getParent() {
        XModelObject p = modelObject.getParent();
        return p == null ? null : new VObjectImpl(p, model);
    }
    
    public String getPath() {
        return modelObject.getPath();
    }
    
    public XModelObject getModelObject() {
        return modelObject;
    }
    
    public long getTimeStamp() {
        return modelObject.getTimeStamp();
    }
    
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o instanceof VObjectImpl) {
            return ((VObjectImpl)o).modelObject.equals(this.modelObject);
        }
        return false;
    }
    
    public int hashCode() {
        return modelObject.hashCode();
    }
    
    public String toString() {
        return org.jboss.tools.common.model.util.FindObjectHelper.makeRef(getPath(),
            org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler.title(modelObject, true));
    }
    
}
