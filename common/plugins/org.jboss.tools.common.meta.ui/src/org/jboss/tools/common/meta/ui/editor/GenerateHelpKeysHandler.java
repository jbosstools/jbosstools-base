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
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.*;

public class GenerateHelpKeysHandler extends AbstractHandler {
    String defpath = null;

    public GenerateHelpKeysHandler() {}

    public boolean isEnabled(XModelObject object) {
        return object != null;
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        defpath = action.getProperty("default"); //$NON-NLS-1$
        XModelObject q = findOrCreateProperties(object);
        XModelObject[] fs = object.getChildren("FileMETA"); //$NON-NLS-1$
        for (int i = 0; i < fs.length; i++) processFile(fs[i], q);
        q.setModified(true);
    }

    private XModelObject findOrCreateProperties(XModelObject p) {
        XModelObject q = p.getChildByPath("keys.properties"); //$NON-NLS-1$
        if(q == null) {
            q = p.getModel().createModelObject("FilePROPERTIES", new Properties()); //$NON-NLS-1$
            q.setAttributeValue("name", "keys"); //$NON-NLS-1$ //$NON-NLS-2$
            q.setAttributeValue("extension", "properties"); //$NON-NLS-1$ //$NON-NLS-2$
            p.addChild(q);
        }
        return q;
    }

    private void validateProperty(XModelObject q, String name, String value) {
        if(q.getChildByPath(name) != null) return;
        XModelObject v = q.getModel().createModelObject("Property", new Properties()); //$NON-NLS-1$
        v.setAttributeValue("name", name); //$NON-NLS-1$
        if(value != null) v.setAttributeValue("value", value); //$NON-NLS-1$
        q.addChild(v);
    }

    private void processFile(XModelObject f, XModelObject q) {
        XModelObject[] es = f.getChildren("MetaEntity"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) processEntity(es[i], q);
        es = f.getChildren("MetaEntityExtension"); //$NON-NLS-1$
        for (int i = 0; i < es.length; i++) processEntityExtension(es[i], q);
    }

    private void processEntity(XModelObject e, XModelObject q) {
        boolean impl = (e.getAttributeValue("implementation").length() > 0); //$NON-NLS-1$
        if(impl) {
//        	validateProperty(q, e.getAttributeValue("name"), defpath);
        }
        processActions(e, q, impl);
    }

    private void processEntityExtension(XModelObject e, XModelObject q) {
        processActions(e, q, true);
    }

    private void processActions(XModelObject e, XModelObject q, boolean impl) {
        XModelObject[] as = collectActions(e);
        String pref = e.getAttributeValue("name") + "_"; //$NON-NLS-1$ //$NON-NLS-2$
        for (int i = 0; i < as.length; i++) processAction(pref, as[i], q);
        if(impl) {
//        	validateProperty(q, pref + "Properties", defpath);
        }
    }

    private XModelObject[] collectActions(XModelObject e) {
        ArrayList l = new ArrayList();
        collectActions(e, l);
        return (XModelObject[])l.toArray(new XModelObject[0]);
    }

    private void collectActions(XModelObject e, ArrayList l) {
        XModelObject[] as = e.getChildren("MetaActionList"); //$NON-NLS-1$
        for (int i = 0; i < as.length; i++) collectActions(as[i], l);
        as = e.getChildren("MetaAction"); //$NON-NLS-1$
        for (int i = 0; i < as.length; i++) l.add(as[i]);
    }

    private void processAction(String pref, XModelObject a, XModelObject q) {
        String n = a.getAttributeValue("name"); //$NON-NLS-1$
        String dn = a.getAttributeValue("display name"); //$NON-NLS-1$
        if(dn.endsWith("...")) dn = dn.substring(0, dn.length() - 3); //$NON-NLS-1$
        if(n.startsWith("Add") || n.startsWith("Create")) { //$NON-NLS-1$ //$NON-NLS-2$
        	Properties p = getProperties(a);
        	String key = p.getProperty("key"); //$NON-NLS-1$
        	if(key == null) {
        		key = pref + n;
        	}
        	String wt = dn;
        	if(!wt.startsWith("Add") && !wt.startsWith("New")) { //$NON-NLS-1$ //$NON-NLS-2$
        		wt = "Add " + wt; //$NON-NLS-1$
        	}
			validateProperty(q, key + ".WindowTitle", wt); //$NON-NLS-1$
			String on = getObjectName(a);
			if(on == null) on = dn;
			validateProperty(q, key + ".Title", on); //$NON-NLS-1$
        } else if(n.equals("Properties")) { //$NON-NLS-1$
        	String key = pref + n;
//			validateProperty(q, key + ".WindowTitle", "Properties");
			XModelObject b = a;
			while(b != null && b.getModelEntity().getName().toLowerCase().indexOf("entity") < 0) { //$NON-NLS-1$
				b = b.getParent();
			}
			String on = (b == null) ? dn : getObjectName2(b);
			if(on == null) on = dn;
			validateProperty(q, key + ".Title", on); //$NON-NLS-1$
        } else if(a.getAttributeValue("wizard").length() > 0) { //$NON-NLS-1$
        	Properties p = getProperties(a);
        	String key = p.getProperty("key"); //$NON-NLS-1$
        	if(key == null) {
        		key = pref + n;
        	}
//            validateProperty(q, key, defpath);
			validateProperty(q, key + ".WindowTitle", dn); //$NON-NLS-1$
			validateProperty(q, key + ".Title", ""); //$NON-NLS-1$ //$NON-NLS-2$
			validateProperty(q, key + ".Message", ""); //$NON-NLS-1$ //$NON-NLS-2$
        } else if(isSpecialWizard(a.getAttributeValue("handler"))) { //$NON-NLS-1$
            int m = a.getChildren().length;
            for (int i = 0; i < m; i++) {
				String key = pref + n + "_" + i; //$NON-NLS-1$
//				validateProperty(q, key, defpath);
				validateProperty(q, key + ".WindowTitle", dn); //$NON-NLS-1$
				validateProperty(q, key + ".Title", ""); //$NON-NLS-1$ //$NON-NLS-2$
				validateProperty(q, key + ".Message", ""); //$NON-NLS-1$ //$NON-NLS-2$
            } 
        } else if(n.indexOf("Edit") >= 0) { //$NON-NLS-1$
			String key = pref + n;
//			validateProperty(q, key, defpath);
			validateProperty(q, key + ".WindowTitle", dn); //$NON-NLS-1$
			validateProperty(q, key + ".Title", ""); //$NON-NLS-1$ //$NON-NLS-2$
			validateProperty(q, key + ".Message", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
    
    private boolean isSpecialWizard(String s) {
        return s.equals("org.jboss.tools.common.meta.action.impl.handlers.DefaultSpecialHandler") //$NON-NLS-1$
               || s.equals("%SpecialWizard%"); //$NON-NLS-1$
    }
    
    private String getObjectName(XModelObject a) {
    	XModelObject[] cs = a.getChildren();
    	if(cs == null || cs.length == 0) return null;
    	String n = cs[0].getAttributeValue("entity name"); //$NON-NLS-1$
    	return getObjectName(n);
    }

    private String getObjectName2(XModelObject a) {
    	return getObjectName(a.getAttributeValue("name")); //$NON-NLS-1$
    }

    private String getObjectName(String n) {
    	StringBuffer result = new StringBuffer();
    	for (int i = 0; i < n.length(); i++) {
    		char ch = n.charAt(i);
    		if(i > 0 && Character.isUpperCase(ch) && !Character.isUpperCase(n.charAt(i - 1))) {
    			result.append(' ');
    		}
    		if(!Character.isDigit(ch) && ch != '.') {
    			result.append(ch);
    		}
    	}
    	return result.toString();
    }

    private Properties getProperties(XModelObject a) {
    	Properties p = new Properties();
    	String ps = a.getAttributeValue("properties"); //$NON-NLS-1$
    	if(ps == null || ps.length() == 0) return p;
    	StringTokenizer st = new StringTokenizer(ps, ";"); //$NON-NLS-1$
    	while(st.hasMoreTokens()) {
    		String t = st.nextToken();
    		int i = t.indexOf('=');
    		if(i < 0) continue;
    		String n = t.substring(0, i);
    		String v = t.substring(i + 1);
    		p.setProperty(n, v);
    	}
    	return p;
    }

}
