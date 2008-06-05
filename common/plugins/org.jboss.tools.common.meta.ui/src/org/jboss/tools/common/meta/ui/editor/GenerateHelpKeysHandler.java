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
        defpath = action.getProperty("default");
        XModelObject q = findOrCreateProperties(object);
        XModelObject[] fs = object.getChildren("FileMETA");
        for (int i = 0; i < fs.length; i++) processFile(fs[i], q);
        q.setModified(true);
    }

    private XModelObject findOrCreateProperties(XModelObject p) {
        XModelObject q = p.getChildByPath("keys.properties");
        if(q == null) {
            q = p.getModel().createModelObject("FilePROPERTIES", new Properties());
            q.setAttributeValue("name", "keys");
            q.setAttributeValue("extension", "properties");
            p.addChild(q);
        }
        return q;
    }

    private void validateProperty(XModelObject q, String name, String value) {
        if(q.getChildByPath(name) != null) return;
        XModelObject v = q.getModel().createModelObject("Property", new Properties());
        v.setAttributeValue("name", name);
        if(value != null) v.setAttributeValue("value", value);
        q.addChild(v);
    }

    private void processFile(XModelObject f, XModelObject q) {
        XModelObject[] es = f.getChildren("MetaEntity");
        for (int i = 0; i < es.length; i++) processEntity(es[i], q);
        es = f.getChildren("MetaEntityExtension");
        for (int i = 0; i < es.length; i++) processEntityExtension(es[i], q);
    }

    private void processEntity(XModelObject e, XModelObject q) {
        boolean impl = (e.getAttributeValue("implementation").length() > 0);
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
        String pref = e.getAttributeValue("name") + "_";
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
        XModelObject[] as = e.getChildren("MetaActionList");
        for (int i = 0; i < as.length; i++) collectActions(as[i], l);
        as = e.getChildren("MetaAction");
        for (int i = 0; i < as.length; i++) l.add(as[i]);
    }

    private void processAction(String pref, XModelObject a, XModelObject q) {
        String n = a.getAttributeValue("name");
        String dn = a.getAttributeValue("display name");
        if(dn.endsWith("...")) dn = dn.substring(0, dn.length() - 3);
        if(n.startsWith("Add") || n.startsWith("Create")) {
        	Properties p = getProperties(a);
        	String key = p.getProperty("key");
        	if(key == null) {
        		key = pref + n;
        	}
        	String wt = dn;
        	if(!wt.startsWith("Add") && !wt.startsWith("New")) {
        		wt = "Add " + wt;
        	}
			validateProperty(q, key + ".WindowTitle", wt);
			String on = getObjectName(a);
			if(on == null) on = dn;
			validateProperty(q, key + ".Title", on);
        } else if(n.equals("Properties")) {
        	String key = pref + n;
//			validateProperty(q, key + ".WindowTitle", "Properties");
			XModelObject b = a;
			while(b != null && b.getModelEntity().getName().toLowerCase().indexOf("entity") < 0) {
				b = b.getParent();
			}
			String on = (b == null) ? dn : getObjectName2(b);
			if(on == null) on = dn;
			validateProperty(q, key + ".Title", on);
        } else if(a.getAttributeValue("wizard").length() > 0) {
        	Properties p = getProperties(a);
        	String key = p.getProperty("key");
        	if(key == null) {
        		key = pref + n;
        	}
//            validateProperty(q, key, defpath);
			validateProperty(q, key + ".WindowTitle", dn);
			validateProperty(q, key + ".Title", "");
			validateProperty(q, key + ".Message", "");
        } else if(isSpecialWizard(a.getAttributeValue("handler"))) {
            int m = a.getChildren().length;
            for (int i = 0; i < m; i++) {
				String key = pref + n + "_" + i;
//				validateProperty(q, key, defpath);
				validateProperty(q, key + ".WindowTitle", dn);
				validateProperty(q, key + ".Title", "");
				validateProperty(q, key + ".Message", "");
            } 
        } else if(n.indexOf("Edit") >= 0) {
			String key = pref + n;
//			validateProperty(q, key, defpath);
			validateProperty(q, key + ".WindowTitle", dn);
			validateProperty(q, key + ".Title", "");
			validateProperty(q, key + ".Message", "");
        }
    }
    
    private boolean isSpecialWizard(String s) {
        return s.equals("org.jboss.tools.common.meta.action.impl.handlers.DefaultSpecialHandler")
               || s.equals("%SpecialWizard%");
    }
    
    private String getObjectName(XModelObject a) {
    	XModelObject[] cs = a.getChildren();
    	if(cs == null || cs.length == 0) return null;
    	String n = cs[0].getAttributeValue("entity name");
    	return getObjectName(n);
    }

    private String getObjectName2(XModelObject a) {
    	return getObjectName(a.getAttributeValue("name"));
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
    	String ps = a.getAttributeValue("properties");
    	if(ps == null || ps.length() == 0) return p;
    	StringTokenizer st = new StringTokenizer(ps, ";");
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
