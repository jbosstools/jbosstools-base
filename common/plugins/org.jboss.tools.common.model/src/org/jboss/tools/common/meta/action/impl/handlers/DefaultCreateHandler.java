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

import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.constraint.*;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.event.ActionDeclinedException;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.undo.*;
import org.jboss.tools.common.model.util.*;

public class DefaultCreateHandler extends AbstractHandler {

    protected static XBundle bundle = XBundle.getInstance();

    public DefaultCreateHandler() {}

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object) || data == null || data.length == 0) return;
        String entity = getEntityName();
        Properties p = extractProperties(data[0]);
        setOtherProperties(object, p);
        XModelObject c = XModelObjectLoaderUtil.createValidObject(object.getModel(), entity, p);
        c = modifyCreatedObject(c);
        addCreatedObject(object, c, prop);
		checkPosition(object, c, prop);
		if(prop != null) prop.put("created", c);
    }
    
    private void checkPosition(XModelObject o, XModelObject c, Properties prop) {
    	if(prop == null || !(prop.get("insertAfter") instanceof Integer)) return;
    	int i = ((Integer)prop.get("insertAfter")).intValue();
    	if(i < 0) return;
    	++i;
    	RegularObjectImpl impl = (RegularObjectImpl)o;
    	impl.move(impl.getIndexOfChild(c), i, true); 
    }

    protected String getEntityName() {
        String n = action.getProperty("entity");
        return (n == null) ? data[0].getModelEntity().getName() : n;
    }

    protected XModelObject modifyCreatedObject(XModelObject o) {
        return o;
    }

    protected static String getMessageById(String id) {
        return bundle.getMessage("model", id);
    }

    public static Properties extractProperties(XEntityData es) {
        Properties p = new Properties();
        XAttributeData[] ads = es.getAttributeData();
        for (int i = 0; i < ads.length; i++) {
            XAttribute a = ads[i].getAttribute();
            String pn = a.getName();
            String pv = extractProperty(ads[i]);
            p.setProperty(pn, pv);
        }
        return p;
    }
    
    public static String extractProperty(XAttributeData ad) {
		XAttribute a = ad.getAttribute();
		String pn = a.getName();
		String pv = ad.getValue();
		if(a.isTrimmable()) pv = (pv == null) ? "" : pv.trim();
		if((pv == null || pv.length() == 0) && ad.getMandatoryFlag())
		  throw new RuntimeException(getReguiredMessage(pn));
		XAttributeConstraint c = ad.getAttribute().getConstraint();
		validateValue(pn, pv, c);
		return pv;
    }
    
	public static String getReguiredMessage(String attributeName) {
		return bundle.getMessage("model", "ATTRIBUTE_REQUIRED", new Object[]{attributeName});
	}

	public static Properties getProperties(XEntityData es) {
		Properties p = new Properties();
		XAttributeData[] ads = es.getAttributeData();
		for (int i = 0; i < ads.length; i++) {
			XAttribute a = ads[i].getAttribute();
			String pn = a.getName();
			String pv = ads[i].getValue();
			if(a.isTrimmable()) pv = (pv == null) ? "" : pv.trim();
			if(pv == null) pv = "";
			p.setProperty(pn, pv);
		}
		return p;
	}
    
    public static String validateAttribute(XAttributeData ad, String pv) {
		XAttribute a = ad.getAttribute();
		String vis = WizardKeys.getAttributeDisplayName(ad, true);
		if(a.isTrimmable()) pv = (pv == null) ? "" : pv.trim();
		if((pv == null || pv.length() == 0) && ad.getMandatoryFlag()) {
  		    return bundle.getMessage("model",	"ATTRIBUTE_REQUIRED", new Object[]{vis});
		}
		XAttributeConstraint c = ad.getAttribute().getConstraint();
		return getConstraintMessage(vis, pv, c);
    }
    
    public static String getConstraintMessage(String name, String value, XAttributeConstraint c) {
		if(c == null || c.getError(value) == null) return null;
		return bundle.getMessage("model", "SET_ATTRIBUTE_FAILURE",
					 new Object[]{name, value, c.getError(value)});
    }

    public static void validateValue(String name, String value, XAttributeConstraint c) {
        String mes = getConstraintMessage(name, value, c);
        if(mes != null) throw new RuntimeException(mes);
    }

    protected void setOtherProperties(XModelObject object, Properties p) {}

    public boolean isEnabled(XModelObject object) {
        return (object != null && object.isObjectEditable());
    }

    public static void addCreatedObject(XModelObject parent, XModelObject child, Properties whereSelect) {
        addCreatedObject(parent, child, true, whereSelect);
    }
    
    public static String getContainsMessage(XModelObject parent, XModelObject child) {
		String pathpart = child.getPathPart();
		XModelObject e = parent.getChildByPath(pathpart);
		if(e != null) {
			if(child.getModelEntity().getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) return null;
			String tp = title(parent, true), tc = title(child, false), te = title(e, false);
			String mes = (tc.equals(te))
						 ? bundle.getMessage("model", "CONTAINS_OBJECT_1",
											 new Object[]{tp, tc})
						 : bundle.getMessage("model", "CONTAINS_OBJECT_2",
											 new Object[]{tp, te, "\n", tc});
			return mes;
		}
		return null;
    }

    public static void addCreatedObject(XModelObject parent, XModelObject child, boolean registerundo, Properties whereSelect) {
    	addCreatedObject(parent, child, registerundo, extractWhereSelect(whereSelect));
    }

    public static void addCreatedObject(XModelObject parent, XModelObject child, int whereSelect) {
    	addCreatedObject(parent, child, true, whereSelect);
    }

    public static void addCreatedObject(XModelObject parent, final XModelObject child, boolean registerundo, final int whereSelect) {
        if(child == null) throw new RuntimeException(getMessageById("OBJECT_CREATION_FAILURE"));
        String mes = getContainsMessage(parent, child);
        if(mes != null) throw new RuntimeException(mes);
        String ce = child.getModelEntity().getName();
        XChild c = parent.getModelEntity().getChild(ce);
        if(c == null) throw new RuntimeException(getMessageById("OBJECT_ADDING_FAILURE"));
        int max = c.getMaxCount();
        int cur = parent.getChildren(ce).length;
        if(cur >= max) {
			ServiceDialog d = parent.getModel().getService();
			d.showDialog("Warning", "The limit of " + max + " children is achieved.", new String[]{"OK"}, null, ServiceDialog.MESSAGE);
            mes = title(parent, true) + " can contain only " + max +
                         ((max == 1) ? " child " : " children ") +
                         "with entity " + ce + ".";
            throw new ActionDeclinedException(mes);
        }
        boolean b = parent.addChild(child);
		if(!b && child.getModelEntity().getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null) {
			int k = 1;
			String pp = child.getPathPart();
			while(parent.getChildByPath(pp) != null) {
				child.setAttributeValue(XModelObjectLoaderUtil.ATTR_ID_NAME, "" + k);
				String ppn = child.getPathPart();
				if(ppn.equals(pp)) throw new RuntimeException(getMessageById("OBJECT_ADDING_FAILURE"));
				pp = ppn;
				++k;
			}
			b = parent.addChild(child);
		}
        if(!b) {
        	throw new RuntimeException(getMessageById("OBJECT_ADDING_FAILURE"));
        }
        XUndoManager undo = getUndoManager(parent);
        if(registerundo && undo != null) {
            undo.addUndoable(new XCreateUndo(parent, child));
        }
        child.setModified(true);
        if(whereSelect >= 0) {
        	Display.getDefault().asyncExec(new Runnable() {
        		public void run() {
                	FindObjectHelper.findModelObject(child, whereSelect);
        		}
        	});
        }
    }

    // now undo works only for ancestors of the model root!
    static XUndoManager getUndoManager(XModelObject o) {
        XModelObject root = o.getModel().getRoot();
        while(o != null && o != root) o = o.getParent();
        return (o == null) ? null : o.getModel().getUndoManager();
    }

    public static String title(XModelObject o, boolean capitalize) {
        String s = o.getAttributeValue("element type") + " " +
                   o.getModelEntity().getRenderer().getTitle(o);
        return (!capitalize || s.length() < 0 || Character.isUpperCase(s.charAt(0)))
               ? s : s.substring(0, 1).toUpperCase() + s.substring(1);
    }
    
    public static int extractWhereSelect(Properties p) {
    	if(p == null) return FindObjectHelper.EVERY_WHERE;
    	String component = p.getProperty("actionSourceGUIComponentID");
    	if("navigator".equals(component)) return FindObjectHelper.IN_NAVIGATOR_AND_IN_EDITOR_IF_OPEN;
    	if("editor".equals(component)) return FindObjectHelper.IN_EDITOR_ONLY;
    	if("dialog".equals(component)) return FindObjectHelper.IN_NAVIGATOR_ONLY;
    	return FindObjectHelper.EVERY_WHERE;
    }

}

