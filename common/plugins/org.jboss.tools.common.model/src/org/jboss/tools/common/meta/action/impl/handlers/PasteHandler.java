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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.model.event.ActionDeclinedException;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class PasteHandler extends AbstractHandler {	
	/**
	 * When more than one object is pasted and paste of each object
	 * invokes a wizard, to cancel paste of current object and all 
	 * subsequent objects, wizard should set this context property
	 * to 'true'.
	 */	
	public static String IS_CANCELLED = "isCancelled";

    public PasteHandler() {
        data = new XEntityDataImpl[1];
    }

    private static String[] attrs = {"attribute name", "yes",
                                     "entity name", "yes",
                                     "parameters", "no"};

    public XEntityData[] getEntityData(XModelObject object) {
        return getEntityData(object, 0);
    }

    protected XEntityData[] getEntityData(XModelObject object, int bi) {
        if(getBuffer(object).source(bi) == null) return null;
        ArrayList<String[]> a = new ArrayList<String[]>();
        a.add(new String[]{getEntityName(object, bi), "yes"});
        String an = getAttributeName(getEntityName(object, bi));
        if(an != null) a.add(new String[]{an, "yes"});
        for (int i = 0; i < attrs.length; i += 2)
          register(object, attrs[i], attrs[i + 1], a, bi);
        String[][] ds = new String[a.size()][];
        for (int i = 0; i < ds.length; i++) ds[i] = (String[])a.get(i);
        data[0] = XEntityDataImpl.create(ds);
        setDefaultData(object, bi);
        return data;
    }

    private void register(XModelObject o, String attr, String mandatory, ArrayList<String[]> list, int bi) {
        if(hasAttribute(o.getModel(), attr, bi)) list.add(new String[]{attr, mandatory});
    }

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        boolean isDrop = (prop != null) && "true".equals(prop.getProperty("isDrop"));
        int bs = object.getModel().getModelBuffer().getSize();
        for (int i = 0; i < bs; i++) {
        	execute(object, i, isDrop, prop);
        	if(prop != null && "true".equals(prop.getProperty(IS_CANCELLED))) return;
        } 
    }

    protected void execute(XModelObject parent, int sourceIndex, boolean isDrop, Properties p) throws Exception {
        XModelBuffer buffer = getBuffer(parent);
        boolean isParent = isParent(parent, buffer.copy(sourceIndex));
        if(isParent) {
			if(!prepaste(parent, sourceIndex)) return;
            if(!isDrop)
              paste(parent, sourceIndex, p);
            else
              pasteOnDrop(parent, sourceIndex, p);
        } else {
            drop(parent, buffer.source(sourceIndex), p);
        }
    }
    
    protected final void paste(XModelObject parent, int sourceIndex, Properties p) {
    	XModelObject source = getBuffer(parent).source(sourceIndex);
        XModelObject copy = getBuffer(parent).copy(sourceIndex);
        XModelObject existing = parent.getChildByPath(copy.getPathPart());
        if(existing != null) {
        	XChild c = parent.getModelEntity().getChild(existing.getModelEntity().getName());
        	if(c != null && c.isRequired() && c.getMaxCount() == 1) return;
        }
        String found = (existing == null) ? null : copy.getModelEntity().getRenderer().getTitle(copy);
        String entity = getEntityName(parent, sourceIndex);
		getEntityData(parent, sourceIndex);
		XAttributeData[] ad = ((XEntityData[])data)[0].getAttributeData();
		boolean mustGenerate = (found != null && mustGenerate(entity));
		if(!mustGenerate) {
			int i = (found == null) ? 0 : PasteEnterNewNameSupport.run(parent, source, copy, data[0]);
			if(i != 0) throw new ActionDeclinedException("Paste declined.");
		}
        for (int j = 0; j < ad.length; j++)
          copy.setAttributeValue(ad[j].getAttribute().getName(), ad[j].getValue());
        if(!mustGenerate) {
        	String an = getAttributeName(getEntityName(parent, sourceIndex));
        	XAttributeData a = (an == null) ? null : HUtil.find(data, 0, an);
        	if(a != null) copy.setAttributeValue(an, a.getValue());
        } else {
        	generate(parent, copy, getAttributeName(entity));
        }
        copy = modify(copy.copy());
        DefaultCreateHandler.addCreatedObject(parent, copy, p);
		if(copy.isActive()) onChildPasted(copy);        
    }
    
	private boolean prepaste(XModelObject parent, int sourceIndex) {
		XModelObject source = parent.getModel().getModelBuffer().source(sourceIndex);
		String entity = source.getModelEntity().getName();
		XChild c = parent.getModelEntity().getChild(entity);
		if(c == null || c.getMaxCount() > 1) return true;  
		XModelObject co = parent.getChildByPath(source.getPathPart());
		if(co == null) return true;
		if(co == source) {
			if(sourceIndex == 0) showUniqueMessae(parent, source);
			return false;
		}
		String message = "Replace existing element ";
		String n = "<" + co.getModelEntity().getXMLSubPath() + ">";
		message += n + "?";
		ServiceDialog d = parent.getModel().getService();
		int q = d.showDialog("Paste", message, new String[]{"OK", "Cancel"}, null, ServiceDialog.QUESTION);
		if(q != 0) return false;
		DefaultRemoveHandler.removeFromParent(co);
		return true;
	}
	private void showUniqueMessae(XModelObject parent, XModelObject source) {
		ServiceDialog d = parent.getModel().getService();
		String message = DefaultCreateHandler.title(parent, true) +
		                 " may have only one " + source.getAttributeValue("element type") + "." +
		                 " You cannot add its copy.";
		d.showDialog("Warning", message, new String[]{SpecialWizardSupport.OK}, null, ServiceDialog.WARNING);
	}

	protected void onChildPasted(XModelObject child) {}

    protected void pasteOnDrop(XModelObject parent, int sourceIndex, Properties p) throws Exception {
        paste(parent, sourceIndex, p);
    }

    protected final void drop(XModelObject parent, XModelObject source, Properties p) throws Exception {
        XAdoptManager am = parent.getModelEntity().getAdoptManager();
        if(am != null) am.adopt(parent, source, p);
    }

    protected XModelObject modify(XModelObject c) {
        return c;
    }

    public boolean isEnabled(XModelObject object) {
        XModelBuffer buffer = object.getModel().getModelBuffer();
        int bs = buffer.getSize();
        if(bs == 0 || object == null || !object.isObjectEditable()) return false;
        for (int i = 0; i < bs; i++) {
            XModelObject s = buffer.source(i);
            boolean b = (s != null && (isParent(object, s) || canAdopt(object, s)));
            if(!b) return false;
        }
        return true;
    }

    protected boolean isParent(XModelObject p, XModelObject c) {
        c = c.copy(true, 0);
        return (c != null) && (p.getModelEntity().getChild(c.getModelEntity().getName()) != null);
    }

    protected boolean canAdopt(XModelObject target, XModelObject bullet) {
        XAdoptManager am = target.getModelEntity().getAdoptManager();
        return (am != null && am.isAdoptable(target, bullet));
    }

    protected String getEntityName(XModelObject object, int i) {
        String entity = action.getProperty("entity");
        if(entity != null) return entity;
        return getBuffer(object).source(i).getModelEntity().getName();
    }

    protected String getAttributeName(String entity) {
    	XModelEntity e = XModelMetaDataImpl.getInstance().getEntity(entity);
    	if(e.getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) return XModelObjectLoaderUtil.ATTR_ID_NAME;
    	XAttribute[] as = e.getAttributes();
    	for (int i = 0; i < as.length; i++) 
    		if("true".equals(as[i].getProperty("id"))) return as[i].getName();
		if(e.getAttribute("name") != null) return "name";
    	return null;
    }
    
    protected boolean mustGenerate(String entity) {
    	XModelEntity e = XModelMetaDataImpl.getInstance().getEntity(entity);
    	if(e.getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) return true;
    	XAttribute[] as = e.getAttributes();
    	for (int i = 0; i < as.length; i++) 
    		if("true".equals(as[i].getProperty("id")) &&
    		   "true".equals(as[i].getProperty("generate"))) return true;
    	return false;
    }
    
    protected int generate(XModelObject parent, XModelObject copy, String attr) {
    	String v = copy.getAttributeValue(attr);
    	while(v.length() > 0 && Character.isDigit(v.charAt(v.length() - 1))) v = v.substring(0, v.length() - 1);
    	int k = 1;
    	String pp = copy.getPathPart();
		while(parent.getChildByPath(pp) != null) {
			copy.setAttributeValue(attr, v + k);
			String ppn = copy.getPathPart();
///			if(ppn.equals(pp)) return 0;
			pp = ppn;
			++k;
		}
    	return 0;
    }

    protected boolean hasAttribute(XModel model, String attr, int bi) {
        return model.getMetaData().getEntity(getEntityName(model.getRoot(), bi)).getAttribute(attr) != null;
    }

    public void setDefaultData(XModelObject object) {
        setDefaultData(object, 0);
    }
    public void setDefaultData(XModelObject object, int bi) {
        if(!isEnabled(object)) return;
        XModelObject c = getBuffer(object).copy(bi);
        XAttributeData[] ad = ((XEntityData[])data)[0].getAttributeData();
        for (int i = 0; i < ad.length; i++)
          ad[i].setValue(c.getAttributeValue(ad[i].getAttribute().getName()));
    }

    public void setData(XEntityData[] data) {}

    private XModelBuffer getBuffer(XModelObject o) {
        return o.getModel().getModelBuffer();
    }

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        return (objects == null || objects.length == 1) && isEnabled(object);
    }

}
